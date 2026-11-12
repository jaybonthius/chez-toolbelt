/* The reader in md4c.sls decodes exactly what this file writes. */

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "md4c.h"
#include "entity.h"

#define EXPORT __attribute__((visibility("default")))

#define NO_OFFSET UINT32_C(0xffffffff)

typedef struct {
  unsigned char *data;
  size_t size;
  size_t capacity;
  int failed;
  const MD_CHAR *document;
  size_t document_size;
  /* Offsets are wanted in characters and arrive almost always in document
     order, so a cursor makes converting them linear in the document. */
  size_t byte_cursor;
  size_t char_cursor;
  size_t *pending;
  size_t pending_count;
  size_t pending_capacity;
} events;

static int grow(events *out, size_t more) {
  if (out->failed) return 0;
  size_t capacity = out->capacity ? out->capacity : 4096;
  while (capacity - out->size < more) {
    if (capacity > SIZE_MAX / 2) { out->failed = 1; return 0; }
    capacity *= 2;
  }
  unsigned char *data = realloc(out->data, capacity);
  if (!data) { out->failed = 1; return 0; }
  out->data = data;
  out->capacity = capacity;
  return 1;
}

static void put_bytes(events *out, const void *bytes, size_t count) {
  if (out->capacity - out->size < count && !grow(out, count)) return;
  memcpy(out->data + out->size, bytes, count);
  out->size += count;
}

static void put_u8(events *out, unsigned value) {
  unsigned char byte = (unsigned char)value;
  put_bytes(out, &byte, 1);
}

static void set_u32(unsigned char *at, uint32_t value) {
  at[0] = value & 0xff;
  at[1] = (value >> 8) & 0xff;
  at[2] = (value >> 16) & 0xff;
  at[3] = (value >> 24) & 0xff;
}

static void put_u32(events *out, uint32_t value) {
  unsigned char bytes[4];
  set_u32(bytes, value);
  put_bytes(out, bytes, 4);
}

static void set_u64(unsigned char *at, uint64_t value) {
  set_u32(at, (uint32_t)value);
  set_u32(at + 4, (uint32_t)(value >> 32));
}

static int in_document(const events *out, const MD_CHAR *text, size_t size) {
  uintptr_t start = (uintptr_t)out->document;
  uintptr_t at = (uintptr_t)text;
  return at >= start && at - start <= out->document_size && size <= out->document_size - (at - start);
}

static uint32_t char_offset(events *out, const MD_CHAR *text) {
  size_t byte = (size_t)(text - out->document);
  const unsigned char *d = (const unsigned char *)out->document;
  while (out->byte_cursor < byte) {
    if ((d[out->byte_cursor] & 0xc0) != 0x80) out->char_cursor++;
    out->byte_cursor++;
  }
  while (out->byte_cursor > byte) {
    out->byte_cursor--;
    if ((d[out->byte_cursor] & 0xc0) != 0x80) out->char_cursor--;
  }
  return (uint32_t)out->char_cursor;
}

static uint32_t char_count(const MD_CHAR *text, size_t size) {
  uint32_t n = 0;
  for (size_t i = 0; i < size; i++)
    if ((((const unsigned char *)text)[i] & 0xc0) != 0x80) n++;
  return n;
}

static void put_code_point(events *out, unsigned cp) {
  unsigned char utf8[4];
  /* CommonMark replaces NUL, surrogates and values past Unicode. */
  if (cp == 0 || cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff)) cp = 0xfffd;
  if (cp <= 0x7f) {
    utf8[0] = cp;
    put_bytes(out, utf8, 1);
  } else if (cp <= 0x7ff) {
    utf8[0] = 0xc0 | (cp >> 6);
    utf8[1] = 0x80 | (cp & 0x3f);
    put_bytes(out, utf8, 2);
  } else if (cp <= 0xffff) {
    utf8[0] = 0xe0 | (cp >> 12);
    utf8[1] = 0x80 | ((cp >> 6) & 0x3f);
    utf8[2] = 0x80 | (cp & 0x3f);
    put_bytes(out, utf8, 3);
  } else {
    utf8[0] = 0xf0 | (cp >> 18);
    utf8[1] = 0x80 | ((cp >> 12) & 0x3f);
    utf8[2] = 0x80 | ((cp >> 6) & 0x3f);
    utf8[3] = 0x80 | (cp & 0x3f);
    put_bytes(out, utf8, 4);
  }
}

static unsigned hex_digit(MD_CHAR c) {
  if (c >= '0' && c <= '9') return (unsigned)(c - '0');
  if (c >= 'a' && c <= 'f') return (unsigned)(c - 'a' + 10);
  return (unsigned)(c - 'A' + 10);
}

/* md4c recognises at most seven decimal or six hexadecimal digits, so the
   value cannot overflow. */
static void put_entity(events *out, const MD_CHAR *text, size_t size) {
  if (size > 3 && text[1] == '#') {
    unsigned cp = 0;
    if (text[2] == 'x' || text[2] == 'X')
      for (size_t i = 3; i < size - 1; i++) cp = 16 * cp + hex_digit(text[i]);
    else
      for (size_t i = 2; i < size - 1; i++) cp = 10 * cp + (unsigned)(text[i] - '0');
    put_code_point(out, cp);
    return;
  }
  const ENTITY *entity = entity_lookup(text, size);
  if (entity) {
    put_code_point(out, entity->codepoints[0]);
    if (entity->codepoints[1]) put_code_point(out, entity->codepoints[1]);
    return;
  }
  put_bytes(out, text, size);
}

static void put_decoded(events *out, MD_TEXTTYPE type, const MD_CHAR *text, size_t size) {
  switch (type) {
  case MD_TEXT_ENTITY: put_entity(out, text, size); break;
  case MD_TEXT_NULLCHAR: put_code_point(out, 0xfffd); break;
  default: put_bytes(out, text, size); break;
  }
}

/* A string's byte count is written once its bytes are, since decoding
   changes it. It takes 64 bits: decoding can triple an attribute, as each
   NUL becomes U+FFFD, past what 32 bits count. */
static size_t begin_string(events *out) {
  put_u32(out, 0);
  put_u32(out, 0);
  return out->size;
}

static void end_string(events *out, size_t start) {
  if (out->failed) return;
  set_u64(out->data + start - 8, (uint64_t)(out->size - start));
}

static void put_attribute(events *out, const MD_ATTRIBUTE *attribute) {
  size_t start = begin_string(out);
  if (attribute->text && attribute->substr_offsets)
    for (int i = 0; attribute->substr_offsets[i] < attribute->size; i++) {
      MD_OFFSET offset = attribute->substr_offsets[i];
      put_decoded(out, attribute->substr_types[i], attribute->text + offset,
                  attribute->substr_offsets[i + 1] - offset);
    }
  end_string(out, start);
}

static uint32_t attribute_offset(events *out, const MD_ATTRIBUTE *attribute) {
  if (!attribute->text || attribute->size == 0 || !in_document(out, attribute->text, attribute->size))
    return NO_OFFSET;
  return char_offset(out, attribute->text);
}

static void add_pending(events *out) {
  if (out->failed) return;
  if (out->pending_count == out->pending_capacity) {
    size_t capacity = out->pending_capacity ? 2 * out->pending_capacity : 16;
    size_t *pending = realloc(out->pending, capacity * sizeof *pending);
    if (!pending) { out->failed = 1; return; }
    out->pending = pending;
    out->pending_capacity = capacity;
  }
  out->pending[out->pending_count++] = out->size;
  put_u32(out, NO_OFFSET);
}

static void resolve_pending(events *out, uint32_t offset) {
  if (out->failed) return;
  for (size_t i = 0; i < out->pending_count; i++) set_u32(out->data + out->pending[i], offset);
  out->pending_count = 0;
}

static int on_enter_block(MD_BLOCKTYPE type, void *detail, void *userdata) {
  events *out = userdata;
  switch (type) {
  case MD_BLOCK_P: put_u8(out, 'P'); break;
  case MD_BLOCK_H:
    put_u8(out, 'H');
    put_u8(out, ((MD_BLOCK_H_DETAIL *)detail)->level);
    break;
  case MD_BLOCK_CODE: {
    MD_BLOCK_CODE_DETAIL *d = detail;
    put_u8(out, 'C');
    put_u8(out, (unsigned char)d->fence_char);
    put_u32(out, attribute_offset(out, &d->info));
    put_attribute(out, &d->info);
    put_attribute(out, &d->lang);
    break;
  }
  case MD_BLOCK_QUOTE: put_u8(out, 'Q'); break;
  case MD_BLOCK_UL:
    put_u8(out, 'U');
    put_u8(out, ((MD_BLOCK_UL_DETAIL *)detail)->is_tight != 0);
    break;
  case MD_BLOCK_OL: {
    MD_BLOCK_OL_DETAIL *d = detail;
    put_u8(out, 'O');
    put_u32(out, d->start);
    put_u8(out, d->is_tight != 0);
    break;
  }
  case MD_BLOCK_LI: {
    MD_BLOCK_LI_DETAIL *d = detail;
    put_u8(out, 'L');
    put_u8(out, d->is_task ? (unsigned char)d->task_mark : 0);
    break;
  }
  case MD_BLOCK_HR: put_u8(out, '-'); break;
  case MD_BLOCK_TABLE: put_u8(out, 'T'); break;
  case MD_BLOCK_TR: put_u8(out, 'R'); break;
  case MD_BLOCK_TH:
  case MD_BLOCK_TD:
    put_u8(out, 'D');
    switch (((MD_BLOCK_TD_DETAIL *)detail)->align) {
    case MD_ALIGN_LEFT: put_u8(out, 'l'); break;
    case MD_ALIGN_CENTER: put_u8(out, 'c'); break;
    case MD_ALIGN_RIGHT: put_u8(out, 'r'); break;
    default: put_u8(out, 0); break;
    }
    break;
  default: break;
  }
  return out->failed;
}

/* Blocks the reader does not see (a table's head and body, and kinds the
   parse flags exclude) leave their contents to their parent. */
static int on_leave_block(MD_BLOCKTYPE type, void *detail, void *userdata) {
  events *out = userdata;
  (void)detail;
  switch (type) {
  case MD_BLOCK_DOC: case MD_BLOCK_P: case MD_BLOCK_H: case MD_BLOCK_CODE: case MD_BLOCK_QUOTE:
  case MD_BLOCK_UL: case MD_BLOCK_OL: case MD_BLOCK_LI: case MD_BLOCK_HR:
  case MD_BLOCK_TABLE: case MD_BLOCK_TR: case MD_BLOCK_TH: case MD_BLOCK_TD:
    put_u8(out, '.');
    break;
  default: break;
  }
  return out->failed;
}

static int on_enter_span(MD_SPANTYPE type, void *detail, void *userdata) {
  events *out = userdata;
  switch (type) {
  case MD_SPAN_EM: put_u8(out, 'e'); break;
  case MD_SPAN_STRONG: put_u8(out, 's'); break;
  case MD_SPAN_DEL: put_u8(out, '~'); break;
  case MD_SPAN_CODE: put_u8(out, '`'); break;
  case MD_SPAN_A: {
    MD_SPAN_A_DETAIL *d = detail;
    put_u8(out, 'a');
    put_attribute(out, &d->href);
    put_attribute(out, &d->title);
    put_u8(out, d->is_autolink != 0);
    break;
  }
  case MD_SPAN_IMG: {
    MD_SPAN_IMG_DETAIL *d = detail;
    put_u8(out, 'i');
    put_attribute(out, &d->src);
    put_attribute(out, &d->title);
    break;
  }
  default: break;
  }
  return out->failed;
}

static int on_leave_span(MD_SPANTYPE type, void *detail, void *userdata) {
  events *out = userdata;
  (void)detail;
  switch (type) {
  case MD_SPAN_EM: case MD_SPAN_STRONG: case MD_SPAN_DEL: case MD_SPAN_CODE:
    put_u8(out, '.');
    break;
  case MD_SPAN_A: case MD_SPAN_IMG:
    put_u8(out, '.');
    add_pending(out);
    break;
  default: break;
  }
  return out->failed;
}

static int on_text(MD_TEXTTYPE type, const MD_CHAR *text, MD_SIZE size, void *userdata) {
  events *out = userdata;
  if (type == MD_TEXT_BR) {
    put_u8(out, 'N');
  } else if (type == MD_TEXT_SOFTBR) {
    put_u8(out, 'n');
  } else if (!in_document(out, text, size)) {
    put_u8(out, 'x');
    put_u32(out, NO_OFFSET);
    put_u32(out, 0);
    size_t start = begin_string(out);
    put_decoded(out, type, text, size);
    end_string(out, start);
  } else {
    uint32_t offset = char_offset(out, text);
    resolve_pending(out, offset);
    if (type == MD_TEXT_ENTITY || type == MD_TEXT_NULLCHAR) {
      put_u8(out, 'x');
      put_u32(out, offset);
      put_u32(out, char_count(text, size));
      size_t start = begin_string(out);
      put_decoded(out, type, text, size);
      end_string(out, start);
    } else {
      put_u8(out, 't');
      put_u32(out, offset);
      put_u32(out, char_count(text, size));
    }
  }
  return out->failed;
}

typedef struct {
  unsigned char *data;
  size_t size;
} parsed;

/* SIZE must be less than 0xffffffff, so that every offset fits in 32 bits
   beside NO_OFFSET. */
EXPORT parsed *chez_md4c_parse(const char *document, size_t size) {
  parsed *result = malloc(sizeof *result);
  if (!result) return NULL;
  events out = {0};
  out.document = document;
  out.document_size = size;
  MD_PARSER parser = {0};
  parser.flags = MD_FLAG_TABLES | MD_FLAG_STRIKETHROUGH | MD_FLAG_TASKLISTS |
                 MD_FLAG_PERMISSIVEAUTOLINKS | MD_FLAG_NOHTML;
  parser.enter_block = on_enter_block;
  parser.leave_block = on_leave_block;
  parser.enter_span = on_enter_span;
  parser.leave_span = on_leave_span;
  parser.text = on_text;
  int status = md_parse(document, (MD_SIZE)size, &parser, &out);
  if (status == 0 && !out.failed)
    resolve_pending(&out, char_offset(&out, document + size));
  free(out.pending);
  if (status != 0 || out.failed) {
    free(out.data);
    free(result);
    return NULL;
  }
  result->data = out.data;
  result->size = out.size;
  return result;
}

EXPORT size_t chez_md4c_size(const parsed *result) {
  return result->size;
}

/* TARGET must hold the result's size in bytes. RESULT is freed. */
EXPORT void chez_md4c_take(parsed *result, unsigned char *target) {
  if (result->size) memcpy(target, result->data, result->size);
  free(result->data);
  free(result);
}
