(library (ghostty ffi)
  (export
    define-c-enum make-c-enum c-enum? c-enum-c-type c-enum-prefix c-enum-entries c-enums
    c-enum->symbol symbol->c-enum
    GhosttyResult GhosttyBuildInfo GhosttyOptimizeMode GhosttyCellContentTag GhosttyCellWide
    GhosttyCellSemanticContent GhosttyCellData GhosttyRowData GhosttyRowSemanticPrompt
    GhosttyClipboardLocation GhosttyClipboardReadResult GhosttyClipboardWriteResult
    GhosttyColorScheme GhosttyFocusEvent GhosttyFormatterFormat GhosttyKey GhosttyKeyAction
    GhosttyKeyEncoderOption GhosttyOptionAsAlt GhosttyKittyGraphicsData
    GhosttyKittyGraphicsImageData GhosttyKittyGraphicsPlacementData GhosttyKittyImageFormat
    GhosttyKittyImageCompression GhosttyModeReportState GhosttyMouseAction GhosttyMouseButton
    GhosttyMouseEncoderOption GhosttyMouseFormat GhosttyMouseShape GhosttyMouseTrackingMode
    GhosttyOscCommandData GhosttyOscCommandType GhosttyOscOption GhosttyOscTerminator
    GhosttyPasteSource
    GhosttyPointTag GhosttyProgramStatusKind GhosttyProgramStatusState
    GhosttyRenderStateCursorVisualStyle GhosttyRenderStateData GhosttyRenderStateDirty
    GhosttyRenderStateOption GhosttyRenderStateRowCellsData GhosttyRenderStateRowData
    GhosttySearchData GhosttySearchOption GhosttySearchScroll GhosttySearchStatus
    GhosttySelectionAdjust GhosttySelectionGestureAutoscroll GhosttySelectionGestureBehavior
    GhosttySelectionGestureData GhosttySelectionGestureEventOption
    GhosttySelectionGestureEventType GhosttySelectionOrder GhosttySemanticPromptKind
    GhosttySemanticPromptPromptKind GhosttySgrAttributeTag GhosttySgrUnderline
    GhosttySizeReportStyle GhosttySnapshotDecoderData GhosttySnapshotDecoderOption
    GhosttyStyleColorTag GhosttyTerminalCompressionMode GhosttyTerminalCompressionResult
    GhosttyTerminalCursorStyle GhosttyTerminalData GhosttyTerminalOption
    GhosttyTerminalProgressState GhosttyTerminalResizeWindowTag GhosttyTerminalScreen
    GhosttyTerminalScrollViewportTag GhosttyTerminalUnknownSequenceTag

    GhosttyString GhosttyBuffer GhosttyCodepoints GhosttyColorRgb GhosttyColorPaletteMask
    GhosttyColorX11Entry GhosttyStyleColor GhosttyStyleColorValue GhosttyStyle
    GhosttyPointCoordinate GhosttyPointValue GhosttyPoint GhosttyGridRef GhosttySelection
    GhosttySelectionBuffer GhosttyTerminalSelectWordOptions
    GhosttyTerminalSelectWordBetweenOptions GhosttyTerminalSelectLineOptions
    GhosttyTerminalSelectionFormatOptions GhosttyFormatterScreenExtra
    GhosttyFormatterTerminalExtra GhosttyFormatterTerminalOptions GhosttyTerminalScrollbar
    GhosttyTerminalMemoryUsage GhosttyTerminalModeConfig GhosttyTerminalScrollViewportValue
    GhosttyTerminalScrollViewport GhosttyTerminalUnknownStringSequence
    GhosttyTerminalUnknownOscSequence GhosttyTerminalUnknownSequenceValue
    GhosttyTerminalUnknownSequence GhosttyClipboardContent GhosttyClipboardWriteReply
    GhosttyClipboardWrite GhosttyClipboardReadReply GhosttyClipboardRead
    GhosttyTerminalDesktopNotification GhosttyTerminalProgressReport
    GhosttyTerminalProgramStatus GhosttyTerminalSemanticPrompt
    GhosttyTerminalResizeWindowGrid GhosttyTerminalResizeWindowValue
    GhosttyTerminalResizeWindow GhosttySizeReportSize GhosttyDeviceAttributesPrimary
    GhosttyDeviceAttributesSecondary GhosttyDeviceAttributesTertiary GhosttyDeviceAttributes
    GhosttyRenderStateRowSelection GhosttyRenderStateCursor GhosttyRenderStateColors
    GhosttyRenderStateOverscan GhosttyRenderStateRowId GhosttyCellsView
    GhosttyMouseEncoderSize GhosttyMousePosition GhosttySurfacePosition
    GhosttySelectionGestureBehaviors GhosttySelectionGestureGeometry GhosttyWriter
    GhosttyMimeReader GhosttyPaste GhosttySgrUnknown GhosttySgrAttributeValue
    GhosttySgrAttribute GhosttyKittyGraphicsPlacementRenderInfo
    c-struct c-sized-struct c-ref c-set! c-ptr ftype-offset
    define-c-packed GhosttyCell

    memcpy-from memcpy-to address->bytevector

    ghostty-type-json ghostty-build-info ghostty-free
    ghostty-terminal-new ghostty-terminal-free ghostty-terminal-reset ghostty-terminal-resize
    ghostty-terminal-set ghostty-terminal-set-value ghostty-terminal-vt-write ghostty-terminal-vt-write-until-ground
    ghostty-terminal-continuation-alloc ghostty-terminal-scroll-viewport
    ghostty-terminal-compression-activity ghostty-terminal-compress ghostty-terminal-get
    ghostty-terminal-grid-ref ghostty-terminal-grid-ref-track
    ghostty-terminal-point-from-grid-ref
    ghostty-grid-ref-cell ghostty-grid-ref-row ghostty-grid-ref-graphemes
    ghostty-grid-ref-hyperlink-uri ghostty-grid-ref-style
    ghostty-tracked-grid-ref-free ghostty-tracked-grid-ref-has-value
    ghostty-tracked-grid-ref-point ghostty-tracked-grid-ref-set
    ghostty-tracked-grid-ref-snapshot
    ghostty-cell-get ghostty-row-get
    ghostty-formatter-terminal-new ghostty-formatter-format-alloc ghostty-formatter-free
    ghostty-terminal-select-word ghostty-terminal-select-word-between
    ghostty-terminal-select-line ghostty-terminal-select-all ghostty-terminal-select-output
    ghostty-terminal-selection-format-alloc ghostty-terminal-selection-adjust
    ghostty-terminal-selection-order ghostty-terminal-selection-contains
    ghostty-selection-gesture-new ghostty-selection-gesture-free
    ghostty-selection-gesture-reset ghostty-selection-gesture-get
    ghostty-selection-gesture-event-new ghostty-selection-gesture-event-free
    ghostty-selection-gesture-event-set ghostty-selection-gesture-event
    ghostty-search-new ghostty-search-free ghostty-search-tick ghostty-search-feed
    ghostty-search-run ghostty-search-set ghostty-search-get
    ghostty-render-state-new ghostty-render-state-free ghostty-render-state-update
    ghostty-render-state-clean ghostty-render-state-get ghostty-render-state-set
    ghostty-render-state-row-iterator-new ghostty-render-state-row-iterator-free
    ghostty-render-state-row-iterator-next ghostty-render-state-row-iterator-next-dirty
    ghostty-render-state-row-get ghostty-render-state-row-cells-new
    ghostty-render-state-row-cells-free ghostty-render-state-row-cells-select
    ghostty-render-state-row-cells-get
    ghostty-snapshot-encode-alloc ghostty-snapshot-decoder-new-buf
    ghostty-snapshot-decoder-free ghostty-snapshot-decoder-set ghostty-snapshot-decoder-ready
    ghostty-snapshot-decoder-next ghostty-snapshot-decoder-decode ghostty-snapshot-decoder-get
    ghostty-key-encoder-new ghostty-key-encoder-free ghostty-key-encoder-setopt
    ghostty-key-encoder-setopt-from-terminal ghostty-key-encoder-encode
    ghostty-key-event-new ghostty-key-event-free ghostty-key-event-set-action
    ghostty-key-event-set-key ghostty-key-event-set-mods ghostty-key-event-set-consumed-mods
    ghostty-key-event-set-composing ghostty-key-event-set-utf8
    ghostty-key-event-set-unshifted-codepoint
    ghostty-mouse-encoder-new ghostty-mouse-encoder-free ghostty-mouse-encoder-setopt
    ghostty-mouse-encoder-setopt-from-terminal ghostty-mouse-encoder-reset
    ghostty-mouse-encoder-encode
    ghostty-mouse-event-new ghostty-mouse-event-free ghostty-mouse-event-set-action
    ghostty-mouse-event-set-button ghostty-mouse-event-clear-button ghostty-mouse-event-set-mods
    ghostty-mouse-event-set-position
    ghostty-osc-new ghostty-osc-free ghostty-osc-set ghostty-osc-next ghostty-osc-end ghostty-osc-command-type
    ghostty-osc-command-data
    ghostty-sgr-new ghostty-sgr-free ghostty-sgr-set-params ghostty-sgr-next
    ghostty-color-parse ghostty-color-parse-x11 ghostty-color-parse-palette-entry
    ghostty-color-palette-default ghostty-color-palette-generate ghostty-color-luminance
    ghostty-color-perceived-luminance ghostty-color-contrast ghostty-color-x11-names
    ghostty-color-x11-name-count
    ghostty-color-scheme-report-encode ghostty-focus-encode ghostty-mode-report-encode
    ghostty-size-report-encode ghostty-paste-is-safe ghostty-paste-encode ghostty-terminal-paste
    ghostty-unicode-codepoint-width ghostty-unicode-grapheme-width
    ghostty-kitty-graphics-get ghostty-kitty-graphics-image ghostty-kitty-graphics-image-get
    ghostty-kitty-graphics-placement-iterator-new ghostty-kitty-graphics-placement-iterator-free
    ghostty-kitty-graphics-placement-next ghostty-kitty-graphics-placement-get
    ghostty-kitty-graphics-placement-render-info)
  (import (chezscheme))

  (include "build/config.ss")

  (define libghostty-vt (load-shared-object libghostty-vt-path))

  ;; (GhosttyKey a) is a constant, so a misspelled name fails at expansion;
  ;; GhosttyKey alone is the table, so tests can check every value against
  ;; the library.
  (define-record-type c-enum
    (nongenerative)
    (sealed #t)
    (fields c-type prefix entries by-symbol by-value))

  (define all-enums '())

  (define (c-enums) all-enums)

  (define (make-enum c-type prefix entries)
    (let ([by-symbol (make-eq-hashtable)] [by-value (make-eqv-hashtable)])
      (for-each
        (lambda (entry)
          (hashtable-set! by-symbol (car entry) (cdr entry))
          (hashtable-set! by-value (cdr entry) (car entry)))
        entries)
      (let ([e (make-c-enum c-type prefix entries by-symbol by-value)])
        (set! all-enums (cons e all-enums))
        e)))

  (define (c-enum->symbol e n) (hashtable-ref (c-enum-by-value e) n #f))

  (define (symbol->c-enum e s who)
    (or (and (symbol? s) (hashtable-ref (c-enum-by-symbol e) s #f))
        (assertion-violationf who "~s is not one of ~s" s (map car (c-enum-entries e)))))

  (define-syntax define-c-enum
    (lambda (x)
      (syntax-case x ()
        [(_ name prefix (symbol value) ...)
         (with-syntax ([c-type (symbol->string (syntax->datum #'name))]
                       [(table) (generate-temporaries '(table))])
           #'(begin
               (define table (make-enum c-type prefix '((symbol . value) ...)))
               (define-syntax name
                 (lambda (y)
                   (syntax-case y ()
                     [id (identifier? #'id) #'table]
                     [(_ s)
                      (let ([entry (assq (syntax->datum #'s) '((symbol . value) ...))])
                        (unless entry (syntax-violation 'name "unknown constant" y #'s))
                        (datum->syntax #'s (cdr entry)))])))))])))

  (define-c-enum GhosttyResult "GHOSTTY_"
    (success 0) (out-of-memory -1) (invalid-value -2) (out-of-space -3) (no-value -4)
    (io-error -5) (limit-exceeded -6) (rejected -7))
  (define-c-enum GhosttyBuildInfo "GHOSTTY_BUILD_INFO_"
    (invalid 0) (simd 1) (kitty-graphics 2) (tmux-control-mode 3) (optimize 4)
    (version-string 5) (version-major 6) (version-minor 7) (version-patch 8) (version-pre 9)
    (version-build 10))
  (define-c-enum GhosttyOptimizeMode "GHOSTTY_OPTIMIZE_"
    (debug 0) (release-safe 1) (release-small 2) (release-fast 3))
  (define-c-enum GhosttyCellContentTag "GHOSTTY_CELL_CONTENT_"
    (codepoint 0) (codepoint-grapheme 1) (bg-color-palette 2) (bg-color-rgb 3))
  (define-c-enum GhosttyCellWide "GHOSTTY_CELL_WIDE_"
    (narrow 0) (wide 1) (spacer-tail 2) (spacer-head 3))
  (define-c-enum GhosttyCellSemanticContent "GHOSTTY_CELL_SEMANTIC_"
    (output 0) (input 1) (prompt 2))
  (define-c-enum GhosttyCellData "GHOSTTY_CELL_DATA_"
    (invalid 0) (codepoint 1) (content-tag 2) (wide 3) (has-text 4) (has-styling 5)
    (style-id 6) (has-hyperlink 7) (protected 8) (semantic-content 9) (color-palette 10)
    (color-rgb 11))
  (define-c-enum GhosttyRowData "GHOSTTY_ROW_DATA_"
    (invalid 0) (wrap 1) (wrap-continuation 2) (grapheme 3) (styled 4) (hyperlink 5)
    (semantic-prompt 6) (kitty-virtual-placeholder 7) (dirty 8))
  (define-c-enum GhosttyRowSemanticPrompt "GHOSTTY_ROW_SEMANTIC_"
    (none 0) (prompt 1) (prompt-continuation 2))
  (define-c-enum GhosttyClipboardLocation "GHOSTTY_CLIPBOARD_LOCATION_"
    (standard 0) (selection 1) (primary 2))
  (define-c-enum GhosttyClipboardReadResult "GHOSTTY_CLIPBOARD_READ_RESULT_"
    (success 0) (denied 1) (unsupported 2) (busy 3) (io-error 4))
  (define-c-enum GhosttyClipboardWriteResult "GHOSTTY_CLIPBOARD_WRITE_RESULT_"
    (success 0) (denied 1) (unsupported 2) (busy 3) (invalid-data 4) (io-error 5))
  (define-c-enum GhosttyColorScheme "GHOSTTY_COLOR_SCHEME_"
    (light 0) (dark 1))
  (define-c-enum GhosttyFocusEvent "GHOSTTY_FOCUS_"
    (gained 0) (lost 1))
  (define-c-enum GhosttyFormatterFormat "GHOSTTY_FORMATTER_FORMAT_"
    (plain 0) (vt 1) (html 2))
  (define-c-enum GhosttyKey "GHOSTTY_KEY_"
    (unidentified 0) (backquote 1) (backslash 2) (bracket-left 3) (bracket-right 4) (comma 5)
    (digit-0 6) (digit-1 7) (digit-2 8) (digit-3 9) (digit-4 10) (digit-5 11) (digit-6 12)
    (digit-7 13) (digit-8 14) (digit-9 15) (equal 16) (intl-backslash 17) (intl-ro 18)
    (intl-yen 19) (a 20) (b 21) (c 22) (d 23) (e 24) (f 25) (g 26) (h 27) (i 28) (j 29) (k 30)
    (l 31) (m 32) (n 33) (o 34) (p 35) (q 36) (r 37) (s 38) (t 39) (u 40) (v 41) (w 42) (x 43)
    (y 44) (z 45) (minus 46) (period 47) (quote 48) (semicolon 49) (slash 50) (alt-left 51)
    (alt-right 52) (backspace 53) (caps-lock 54) (context-menu 55) (control-left 56)
    (control-right 57) (enter 58) (meta-left 59) (meta-right 60) (shift-left 61)
    (shift-right 62) (space 63) (tab 64) (convert 65) (kana-mode 66) (non-convert 67)
    (delete 68) (end 69) (help 70) (home 71) (insert 72) (page-down 73) (page-up 74)
    (arrow-down 75) (arrow-left 76) (arrow-right 77) (arrow-up 78) (num-lock 79) (numpad-0 80)
    (numpad-1 81) (numpad-2 82) (numpad-3 83) (numpad-4 84) (numpad-5 85) (numpad-6 86)
    (numpad-7 87) (numpad-8 88) (numpad-9 89) (numpad-add 90) (numpad-backspace 91)
    (numpad-clear 92) (numpad-clear-entry 93) (numpad-comma 94) (numpad-decimal 95)
    (numpad-divide 96) (numpad-enter 97) (numpad-equal 98) (numpad-memory-add 99)
    (numpad-memory-clear 100) (numpad-memory-recall 101) (numpad-memory-store 102)
    (numpad-memory-subtract 103) (numpad-multiply 104) (numpad-paren-left 105)
    (numpad-paren-right 106) (numpad-subtract 107) (numpad-separator 108) (numpad-up 109)
    (numpad-down 110) (numpad-right 111) (numpad-left 112) (numpad-begin 113) (numpad-home 114)
    (numpad-end 115) (numpad-insert 116) (numpad-delete 117) (numpad-page-up 118)
    (numpad-page-down 119) (escape 120) (f1 121) (f2 122) (f3 123) (f4 124) (f5 125) (f6 126)
    (f7 127) (f8 128) (f9 129) (f10 130) (f11 131) (f12 132) (f13 133) (f14 134) (f15 135)
    (f16 136) (f17 137) (f18 138) (f19 139) (f20 140) (f21 141) (f22 142) (f23 143) (f24 144)
    (f25 145) (fn 146) (fn-lock 147) (print-screen 148) (scroll-lock 149) (pause 150)
    (browser-back 151) (browser-favorites 152) (browser-forward 153) (browser-home 154)
    (browser-refresh 155) (browser-search 156) (browser-stop 157) (eject 158)
    (launch-app-1 159) (launch-app-2 160) (launch-mail 161) (media-play-pause 162)
    (media-select 163) (media-stop 164) (media-track-next 165) (media-track-previous 166)
    (power 167) (sleep 168) (audio-volume-down 169) (audio-volume-mute 170)
    (audio-volume-up 171) (wake-up 172) (copy 173) (cut 174) (paste 175))
  (define-c-enum GhosttyKeyAction "GHOSTTY_KEY_ACTION_"
    (release 0) (press 1) (repeat 2))
  (define-c-enum GhosttyKeyEncoderOption "GHOSTTY_KEY_ENCODER_OPT_"
    (cursor-key-application 0) (keypad-key-application 1) (ignore-keypad-with-numlock 2)
    (alt-esc-prefix 3) (modify-other-keys-state-2 4) (kitty-flags 5) (macos-option-as-alt 6)
    (backarrow-key-mode 7))
  (define-c-enum GhosttyOptionAsAlt "GHOSTTY_OPTION_AS_ALT_"
    (false 0) (true 1) (left 2) (right 3))
  (define-c-enum GhosttyKittyGraphicsData "GHOSTTY_KITTY_GRAPHICS_DATA_"
    (invalid 0) (placement-iterator 1) (generation 2))
  (define-c-enum GhosttyKittyGraphicsImageData "GHOSTTY_KITTY_IMAGE_DATA_"
    (invalid 0) (id 1) (number 2) (width 3) (height 4) (format 5) (compression 6) (data-ptr 7)
    (data-len 8) (generation 9))
  (define-c-enum GhosttyKittyGraphicsPlacementData "GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_"
    (invalid 0) (image-id 1) (placement-id 2) (is-virtual 3) (x-offset 4) (y-offset 5)
    (source-x 6) (source-y 7) (source-width 8) (source-height 9) (columns 10) (rows 11) (z 12))
  (define-c-enum GhosttyKittyImageFormat "GHOSTTY_KITTY_IMAGE_FORMAT_"
    (rgb 0) (rgba 1) (png 2) (gray-alpha 3) (gray 4))
  (define-c-enum GhosttyKittyImageCompression "GHOSTTY_KITTY_IMAGE_COMPRESSION_"
    (none 0) (zlib-deflate 1))
  (define-c-enum GhosttyModeReportState "GHOSTTY_MODE_REPORT_"
    (not-recognized 0) (set 1) (reset 2) (permanently-set 3) (permanently-reset 4))
  (define-c-enum GhosttyMouseAction "GHOSTTY_MOUSE_ACTION_"
    (press 0) (release 1) (motion 2))
  (define-c-enum GhosttyMouseButton "GHOSTTY_MOUSE_BUTTON_"
    (unknown 0) (left 1) (right 2) (middle 3) (four 4) (five 5) (six 6) (seven 7) (eight 8)
    (nine 9) (ten 10) (eleven 11))
  (define-c-enum GhosttyMouseEncoderOption "GHOSTTY_MOUSE_ENCODER_OPT_"
    (event 0) (format 1) (size 2) (any-button-pressed 3) (track-last-cell 4))
  (define-c-enum GhosttyMouseFormat "GHOSTTY_MOUSE_FORMAT_"
    (x10 0) (utf8 1) (sgr 2) (urxvt 3) (sgr-pixels 4))
  (define-c-enum GhosttyMouseShape "GHOSTTY_MOUSE_SHAPE_"
    (default 0) (context-menu 1) (help 2) (pointer 3) (progress 4) (wait 5) (cell 6)
    (crosshair 7) (text 8) (vertical-text 9) (alias 10) (copy 11) (move 12) (no-drop 13)
    (not-allowed 14) (grab 15) (grabbing 16) (all-scroll 17) (col-resize 18) (row-resize 19)
    (n-resize 20) (e-resize 21) (s-resize 22) (w-resize 23) (ne-resize 24) (nw-resize 25)
    (se-resize 26) (sw-resize 27) (ew-resize 28) (ns-resize 29) (nesw-resize 30)
    (nwse-resize 31) (zoom-in 32) (zoom-out 33))
  (define-c-enum GhosttyMouseTrackingMode "GHOSTTY_MOUSE_TRACKING_"
    (none 0) (x10 1) (normal 2) (button 3) (any 4))
  (define-c-enum GhosttyOscCommandData "GHOSTTY_OSC_DATA_"
    (invalid 0) (change-window-title-str 1) (unknown-content 2) (unknown-truncated 3)
    (unknown-terminator 4))
  (define-c-enum GhosttyOscCommandType "GHOSTTY_OSC_COMMAND_"
    (invalid 0) (change-window-title 1) (change-window-icon 2) (semantic-prompt 3)
    (clipboard-contents 4) (report-pwd 5) (mouse-shape 6) (color-operation 7)
    (kitty-color-protocol 8) (show-desktop-notification 9) (hyperlink-start 10)
    (hyperlink-end 11) (conemu-sleep 12) (conemu-show-message-box 13)
    (conemu-change-tab-title 14) (conemu-progress-report 15) (conemu-wait-input 16)
    (conemu-guimacro 17) (conemu-run-process 18) (conemu-output-environment-variable 19)
    (conemu-xterm-emulation 20) (conemu-comment 21) (kitty-text-sizing 22)
    (kitty-clipboard-protocol 23) (kitty-dnd-protocol 24) (hierarchical-context 25)
    (kitty-desktop-notification 26) (unknown 27) (program-status 28))
  (define-c-enum GhosttyOscOption "GHOSTTY_OSC_OPT_"
    (unknown-max-bytes 0))
  (define-c-enum GhosttyOscTerminator "GHOSTTY_OSC_TERMINATOR_"
    (st 0) (bel 1))
  (define-c-enum GhosttyPasteSource "GHOSTTY_PASTE_SOURCE_"
    (clipboard 0) (text 1))
  (define-c-enum GhosttyPointTag "GHOSTTY_POINT_TAG_"
    (active 0) (viewport 1) (screen 2) (history 3))
  (define-c-enum GhosttyProgramStatusKind "GHOSTTY_PROGRAM_STATUS_KIND_"
    (none 0) (permission 1) (question 2) (auth 3))
  (define-c-enum GhosttyProgramStatusState "GHOSTTY_PROGRAM_STATUS_STATE_"
    (idle 0) (working 1) (done 2) (blocked 3) (error 4) (clear 5))
  (define-c-enum GhosttyRenderStateCursorVisualStyle "GHOSTTY_RENDER_STATE_CURSOR_VISUAL_STYLE_"
    (bar 0) (block 1) (underline 2) (block-hollow 3))
  (define-c-enum GhosttyRenderStateData "GHOSTTY_RENDER_STATE_DATA_"
    (invalid 0) (cols 1) (rows 2) (dirty 3) (row-iterator 4) (color-background 5)
    (color-foreground 6) (color-cursor 7) (color-cursor-has-value 8) (color-palette 9)
    (cursor-visual-style 10) (cursor-visible 11) (cursor-blinking 12)
    (cursor-password-input 13) (cursor-viewport-has-value 14) (cursor-viewport-x 15)
    (cursor-viewport-y 16) (cursor-viewport-wide-tail 17) (cursor 18) (colors 19) (overscan 20)
    (overscan-request 21))
  (define-c-enum GhosttyRenderStateDirty "GHOSTTY_RENDER_STATE_DIRTY_"
    (false 0) (partial 1) (full 2))
  (define-c-enum GhosttyRenderStateOption "GHOSTTY_RENDER_STATE_OPTION_"
    (dirty 0) (overscan 1))
  (define-c-enum GhosttyRenderStateRowCellsData "GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_"
    (invalid 0) (raw 1) (style 2) (graphemes-len 3) (graphemes-buf 4) (bg-color 5) (fg-color 6)
    (selected 7) (has-styling 8) (graphemes-utf8 9))
  (define-c-enum GhosttyRenderStateRowData "GHOSTTY_RENDER_STATE_ROW_DATA_"
    (invalid 0) (dirty 1) (raw 2) (cells 3) (selection 4) (cells-raw 5) (viewport-y 6) (id 7))
  (define-c-enum GhosttySearchData "GHOSTTY_SEARCH_DATA_"
    (status 0) (needle 1) (total-matches 2) (selected-index 3) (selected-match 4) (matches 5)
    (viewport-matches 6) (select-scroll 7))
  (define-c-enum GhosttySearchOption "GHOSTTY_SEARCH_OPT_"
    (needle 0) (select-next 1) (select-prev 2) (select-scroll 3))
  (define-c-enum GhosttySearchScroll "GHOSTTY_SEARCH_SCROLL_"
    (if-needed 0) (none 1))
  (define-c-enum GhosttySearchStatus "GHOSTTY_SEARCH_STATUS_"
    (running 0) (feed-required 1) (complete 2))
  (define-c-enum GhosttySelectionAdjust "GHOSTTY_SELECTION_ADJUST_"
    (left 0) (right 1) (up 2) (down 3) (home 4) (end 5) (page-up 6) (page-down 7)
    (beginning-of-line 8) (end-of-line 9))
  (define-c-enum GhosttySelectionGestureAutoscroll "GHOSTTY_SELECTION_GESTURE_AUTOSCROLL_"
    (none 0) (up 1) (down 2))
  (define-c-enum GhosttySelectionGestureBehavior "GHOSTTY_SELECTION_GESTURE_BEHAVIOR_"
    (cell 0) (word 1) (line 2) (output 3))
  (define-c-enum GhosttySelectionGestureData "GHOSTTY_SELECTION_GESTURE_DATA_"
    (click-count 0) (dragged 1) (autoscroll 2) (behavior 3) (anchor 4))
  (define-c-enum GhosttySelectionGestureEventOption "GHOSTTY_SELECTION_GESTURE_EVENT_OPT_"
    (ref 0) (position 1) (repeat-distance 2) (time-ns 3) (repeat-interval-ns 4)
    (word-boundary-codepoints 5) (behaviors 6) (rectangle 7) (geometry 8) (viewport 9))
  (define-c-enum GhosttySelectionGestureEventType "GHOSTTY_SELECTION_GESTURE_EVENT_TYPE_"
    (press 0) (release 1) (drag 2) (autoscroll-tick 3) (deep-press 4))
  (define-c-enum GhosttySelectionOrder "GHOSTTY_SELECTION_ORDER_"
    (forward 0) (reverse 1) (mirrored-forward 2) (mirrored-reverse 3))
  (define-c-enum GhosttySemanticPromptKind "GHOSTTY_SEMANTIC_PROMPT_"
    (invalid 0) (prompt-start 1) (input-start 2) (output-start 3) (command-end 4))
  (define-c-enum GhosttySemanticPromptPromptKind "GHOSTTY_SEMANTIC_PROMPT_PROMPT_"
    (primary 0) (right 1) (continuation 2) (secondary 3))
  (define-c-enum GhosttySgrAttributeTag "GHOSTTY_SGR_ATTR_"
    (unset 0) (unknown 1) (bold 2) (reset-bold 3) (italic 4) (reset-italic 5) (faint 6)
    (underline 7) (underline-color 8) (underline-color-256 9) (reset-underline-color 10)
    (overline 11) (reset-overline 12) (blink 13) (reset-blink 14) (inverse 15)
    (reset-inverse 16) (invisible 17) (reset-invisible 18) (strikethrough 19)
    (reset-strikethrough 20) (direct-color-fg 21) (direct-color-bg 22) (bg-8 23) (fg-8 24)
    (reset-fg 25) (reset-bg 26) (bright-bg-8 27) (bright-fg-8 28) (bg-256 29) (fg-256 30))
  (define-c-enum GhosttySgrUnderline "GHOSTTY_SGR_UNDERLINE_"
    (none 0) (single 1) (double 2) (curly 3) (dotted 4) (dashed 5))
  (define-c-enum GhosttySizeReportStyle "GHOSTTY_SIZE_REPORT_"
    (mode-2048 0) (csi-14-t 1) (csi-16-t 2) (csi-18-t 3) (csi-19-t 4))
  (define-c-enum GhosttySnapshotDecoderData "GHOSTTY_SNAPSHOT_DECODER_DATA_"
    (invalid 0) (max-continuation-bytes 1) (source-offset 2) (history-rows-primary 3)
    (history-rows-alternate 4) (progress-screen 5) (progress-rows 6) (progress-remaining 7)
    (retain-continuation 8) (compress-history 9))
  (define-c-enum GhosttySnapshotDecoderOption "GHOSTTY_SNAPSHOT_DECODER_OPT_"
    (max-continuation-bytes 0) (retain-continuation 1) (compress-history 2))
  (define-c-enum GhosttyStyleColorTag "GHOSTTY_STYLE_COLOR_"
    (none 0) (palette 1) (rgb 2))
  (define-c-enum GhosttyTerminalCompressionMode "GHOSTTY_TERMINAL_COMPRESSION_MODE_"
    (incremental 0) (full 1))
  (define-c-enum GhosttyTerminalCompressionResult "GHOSTTY_TERMINAL_COMPRESSION_RESULT_"
    (unsupported 0) (pending 1) (complete 2))
  (define-c-enum GhosttyTerminalCursorStyle "GHOSTTY_TERMINAL_CURSOR_STYLE_"
    (bar 0) (block 1) (underline 2) (block-hollow 3))
  (define-c-enum GhosttyTerminalData "GHOSTTY_TERMINAL_DATA_"
    (invalid 0) (cols 1) (rows 2) (cursor-x 3) (cursor-y 4) (cursor-pending-wrap 5)
    (active-screen 6) (cursor-visible 7) (kitty-keyboard-flags 8) (scrollbar 9)
    (cursor-style 10) (mouse-tracking 11) (title 12) (pwd 13) (total-rows 14)
    (scrollback-rows 15) (width-px 16) (height-px 17) (color-foreground 18)
    (color-background 19) (color-cursor 20) (color-palette 21) (color-foreground-default 22)
    (color-background-default 23) (color-cursor-default 24) (color-palette-default 25)
    (kitty-image-storage-limit 26) (kitty-image-medium-file 27)
    (kitty-image-medium-temp-file 28) (kitty-image-medium-shared-mem 29) (kitty-graphics 30)
    (selection 31) (viewport-active 32) (vt-processing-error 33) (scrollback-max-bytes 34)
    (scrollback-max-lines 35) (continuation-max-bytes 36) (mode 37) (vt-ground 38)
    (cursor-at-prompt 39) (clipboard-write-max-bytes 40) (mouse-shape 41) (memory-usage 42))
  (define-c-enum GhosttyTerminalOption "GHOSTTY_TERMINAL_OPT_"
    (userdata 0) (write-pty 1) (bell 2) (enquiry 3) (xtversion 4) (title-changed 5) (size 6)
    (color-scheme 7) (device-attributes 8) (title 9) (pwd 10) (color-foreground 11)
    (color-background 12) (color-cursor 13) (color-palette 14) (kitty-image-storage-limit 15)
    (kitty-image-medium-file 16) (kitty-image-medium-temp-file 17)
    (kitty-image-medium-shared-mem 18) (apc-max-bytes 19) (apc-max-bytes-kitty 20)
    (selection 21) (default-cursor-style 22) (default-cursor-blink 23) (glyph-protocol 24)
    (pwd-changed 25) (clipboard-write 26) (scrollback-max-bytes 27) (scrollback-max-lines 28)
    (desktop-notification 29) (progress-report 30) (continuation-max-bytes 31)
    (title-report 32) (mode-default 33) (mode 34) (unknown-sequence 35) (unknown-max-bytes 36)
    (terminfo-name 37) (clipboard-read 38) (clipboard-write-max-bytes 39)
    (resize-pull-scrollback 40) (render-hold 41) (semantic-prompt 42) (reset 43)
    (xt-checksum-report 44) (xt-checksum-extension 45) (program-status 46) (resize-window 47))
  (define-c-enum GhosttyTerminalProgressState "GHOSTTY_TERMINAL_PROGRESS_STATE_"
    (remove 0) (set 1) (error 2) (indeterminate 3) (pause 4))
  (define-c-enum GhosttyTerminalResizeWindowTag "GHOSTTY_TERMINAL_RESIZE_WINDOW_"
    (grid 0))
  (define-c-enum GhosttyTerminalScreen "GHOSTTY_TERMINAL_SCREEN_"
    (primary 0) (alternate 1))
  (define-c-enum GhosttyTerminalScrollViewportTag "GHOSTTY_SCROLL_VIEWPORT_"
    (top 0) (bottom 1) (delta 2) (row 3) (delta-prompt 4))
  (define-c-enum GhosttyTerminalUnknownSequenceTag "GHOSTTY_TERMINAL_UNKNOWN_SEQUENCE_"
    (apc 0) (osc 1))

  ;; As with enumerations, GhosttyCell alone is the layout as data, for the
  ;; tests to check.
  (define-syntax define-c-packed
    (syntax-rules ()
      [(_ name (field lsb width) ...)
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [id (identifier? #'id) #''((field lsb width) ...)]
             [(_ f value)
              (let ([entry (assq (syntax->datum #'f) '((field lsb width) ...))])
                (unless entry (syntax-violation 'name "unknown field" x #'f))
                (with-syntax ([shift (cadr entry)] [mask (- (expt 2 (caddr entry)) 1)])
                  #'(fxand (fxsrl value shift) mask)))])))]))

  ;; Bit positions are not part of the ABI; these are the pinned library's,
  ;; from its type manifest, and the ABI test checks them against it.
  (define-c-packed GhosttyCell
    (content_tag 0 2) (content.codepoint 2 21) (content.index 2 8) (content.rgb 2 24)
    (style_id 26 16) (wide 42 2) (protected 44 1) (hyperlink 45 1) (semantic_content 46 2))

  (define-ftype GhosttyString (struct [ptr uptr] [len size_t]))
  (define-ftype GhosttyBuffer (struct [ptr uptr] [cap size_t] [len size_t]))
  (define-ftype GhosttyCodepoints (struct [ptr uptr] [len size_t]))
  (define-ftype GhosttyColorRgb (struct [r unsigned-8] [g unsigned-8] [b unsigned-8]))
  (define-ftype GhosttyColorPaletteMask (struct [bits (array 4 unsigned-64)]))
  (define-ftype GhosttyColorX11Entry (struct [name uptr] [color GhosttyColorRgb]))
  (define-ftype GhosttyStyleColorValue
    (union [palette unsigned-8] [rgb GhosttyColorRgb] [_padding unsigned-64]))
  (define-ftype GhosttyStyleColor (struct [tag int] [value GhosttyStyleColorValue]))
  (define-ftype GhosttyStyle
    (struct
      [size size_t]
      [fg_color GhosttyStyleColor]
      [bg_color GhosttyStyleColor]
      [underline_color GhosttyStyleColor]
      [bold stdbool]
      [italic stdbool]
      [faint stdbool]
      [blink stdbool]
      [inverse stdbool]
      [invisible stdbool]
      [strikethrough stdbool]
      [overline stdbool]
      [underline int]))
  (define-ftype GhosttyPointCoordinate (struct [x unsigned-16] [y unsigned-32]))
  (define-ftype GhosttyPointValue
    (union [coordinate GhosttyPointCoordinate] [_padding (array 2 unsigned-64)]))
  (define-ftype GhosttyPoint (struct [tag int] [value GhosttyPointValue]))
  (define-ftype GhosttyGridRef
    (struct [size size_t] [node uptr] [x unsigned-16] [y unsigned-16]))
  (define-ftype GhosttySelection
    (struct [size size_t] [start GhosttyGridRef] [end GhosttyGridRef] [rectangle stdbool]))
  (define-ftype GhosttySelectionBuffer (struct [ptr uptr] [cap size_t] [len size_t]))
  (define-ftype GhosttyTerminalSelectWordOptions
    (struct
      [size size_t]
      [ref GhosttyGridRef]
      [boundary_codepoints uptr]
      [boundary_codepoints_len size_t]))
  (define-ftype GhosttyTerminalSelectWordBetweenOptions
    (struct
      [size size_t]
      [start GhosttyGridRef]
      [end GhosttyGridRef]
      [boundary_codepoints uptr]
      [boundary_codepoints_len size_t]))
  (define-ftype GhosttyTerminalSelectLineOptions
    (struct
      [size size_t]
      [ref GhosttyGridRef]
      [whitespace uptr]
      [whitespace_len size_t]
      [semantic_prompt_boundary stdbool]))
  (define-ftype GhosttyTerminalSelectionFormatOptions
    (struct [size size_t] [emit int] [unwrap stdbool] [trim stdbool] [selection uptr]))
  (define-ftype GhosttyFormatterScreenExtra
    (struct
      [size size_t]
      [cursor stdbool]
      [style stdbool]
      [hyperlink stdbool]
      [protection stdbool]
      [kitty_keyboard stdbool]
      [charsets stdbool]))
  (define-ftype GhosttyFormatterTerminalExtra
    (struct
      [size size_t]
      [palette stdbool]
      [modes stdbool]
      [scrolling_region stdbool]
      [tabstops stdbool]
      [pwd stdbool]
      [keyboard stdbool]
      [screen GhosttyFormatterScreenExtra]))
  (define-ftype GhosttyFormatterTerminalOptions
    (struct
      [size size_t]
      [emit int]
      [unwrap stdbool]
      [trim stdbool]
      [extra GhosttyFormatterTerminalExtra]
      [selection uptr]))
  (define-ftype GhosttyTerminalScrollbar
    (struct [total unsigned-64] [offset unsigned-64] [len unsigned-64]))
  (define-ftype GhosttyTerminalMemoryUsage
    (struct
      [size size_t]
      [compression_supported stdbool]
      [primary_pages unsigned-64]
      [primary_virtual_bytes unsigned-64]
      [primary_resident_bytes unsigned-64]
      [primary_compressed_pages unsigned-64]
      [primary_compressed_bytes unsigned-64]
      [primary_image_bytes unsigned-64]
      [alternate_pages unsigned-64]
      [alternate_virtual_bytes unsigned-64]
      [alternate_resident_bytes unsigned-64]
      [alternate_compressed_pages unsigned-64]
      [alternate_compressed_bytes unsigned-64]
      [alternate_image_bytes unsigned-64]))
  (define-ftype GhosttyTerminalModeConfig (struct [mode unsigned-16] [value stdbool]))
  (define-ftype GhosttyTerminalScrollViewportValue
    (union [delta iptr] [row size_t] [delta_prompt iptr] [_padding (array 2 unsigned-64)]))
  (define-ftype GhosttyTerminalScrollViewport
    (struct [tag int] [value GhosttyTerminalScrollViewportValue]))
  (define-ftype GhosttyTerminalUnknownStringSequence
    (struct [truncated stdbool] [content GhosttyString]))
  (define-ftype GhosttyTerminalUnknownOscSequence
    (struct [truncated stdbool] [content GhosttyString] [terminator int]))
  (define-ftype GhosttyTerminalUnknownSequenceValue
    (union
      [apc GhosttyTerminalUnknownStringSequence]
      [osc GhosttyTerminalUnknownOscSequence]
      [_padding (array 16 unsigned-64)]))
  (define-ftype GhosttyTerminalUnknownSequence
    (struct [tag int] [value GhosttyTerminalUnknownSequenceValue]))
  (define-ftype GhosttyClipboardContent (struct [mime GhosttyString] [data GhosttyString]))
  (define-ftype GhosttyClipboardWriteReply
    (struct [size size_t] [result int] [remember stdbool]))
  (define-ftype GhosttyClipboardWrite
    (struct
      [size size_t]
      [location int]
      [contents uptr]
      [contents_len size_t]
      [name GhosttyString]
      [granted stdbool]
      [can_remember stdbool]
      [ctx uptr]
      [reply uptr]))
  (define-ftype GhosttyClipboardReadReply
    (struct
      [size size_t]
      [result int]
      [contents uptr]
      [contents_len size_t]
      [available uptr]
      [available_len size_t]
      [remember stdbool]))
  (define-ftype GhosttyClipboardRead
    (struct
      [size size_t]
      [location int]
      [mimes uptr]
      [mimes_len size_t]
      [list stdbool]
      [name GhosttyString]
      [granted stdbool]
      [can_remember stdbool]
      [ctx uptr]
      [reply uptr]))
  (define-ftype GhosttyTerminalDesktopNotification
    (struct [size size_t] [title GhosttyString] [body GhosttyString]))
  (define-ftype GhosttyTerminalProgressReport
    (struct [size size_t] [state int] [progress integer-8]))
  (define-ftype GhosttyTerminalProgramStatus
    (struct
      [size size_t]
      [state int]
      [kind int]
      [progress integer-8]
      [id GhosttyString]
      [app GhosttyString]
      [title GhosttyString]
      [message GhosttyString]))
  (define-ftype GhosttyTerminalSemanticPrompt
    (struct
      [size size_t]
      [kind int]
      [prompt_kind int]
      [has_exit_code stdbool]
      [exit_code integer-32]
      [command GhosttyString]
      [error GhosttyString]))
  (define-ftype GhosttyTerminalResizeWindowGrid (struct [rows unsigned-16] [columns unsigned-16]))
  (define-ftype GhosttyTerminalResizeWindowValue
    (union [grid GhosttyTerminalResizeWindowGrid] [_padding (array 2 unsigned-64)]))
  (define-ftype GhosttyTerminalResizeWindow
    (struct [tag int] [value GhosttyTerminalResizeWindowValue]))
  (define-ftype GhosttySizeReportSize
    (struct
      [rows unsigned-16]
      [columns unsigned-16]
      [cell_width unsigned-32]
      [cell_height unsigned-32]))
  (define-ftype GhosttyDeviceAttributesPrimary
    (struct
      [conformance_level unsigned-16]
      [features (array 64 unsigned-16)]
      [num_features size_t]))
  (define-ftype GhosttyDeviceAttributesSecondary
    (struct
      [device_type unsigned-16]
      [firmware_version unsigned-16]
      [rom_cartridge unsigned-16]))
  (define-ftype GhosttyDeviceAttributesTertiary (struct [unit_id unsigned-32]))
  (define-ftype GhosttyDeviceAttributes
    (struct
      [primary GhosttyDeviceAttributesPrimary]
      [secondary GhosttyDeviceAttributesSecondary]
      [tertiary GhosttyDeviceAttributesTertiary]))
  (define-ftype GhosttyRenderStateRowSelection
    (struct [size size_t] [start_x unsigned-16] [end_x unsigned-16]))
  (define-ftype GhosttyRenderStateCursor
    (struct
      [size size_t]
      [viewport_has_value stdbool]
      [viewport_x unsigned-16]
      [viewport_y unsigned-16]
      [wide_tail stdbool]
      [visible stdbool]
      [blinking stdbool]
      [password_input stdbool]
      [visual_style int]))
  (define-ftype GhosttyRenderStateColors
    (struct
      [size size_t]
      [background GhosttyColorRgb]
      [foreground GhosttyColorRgb]
      [cursor GhosttyColorRgb]
      [cursor_has_value stdbool]
      [palette (array 256 GhosttyColorRgb)]))
  (define-ftype GhosttyRenderStateOverscan (struct [above unsigned-16] [below unsigned-16]))
  (define-ftype GhosttyRenderStateRowId (struct [bits (array 2 unsigned-64)]))
  (define-ftype GhosttyCellsView (struct [ptr uptr] [len size_t]))
  (define-ftype GhosttyMouseEncoderSize
    (struct
      [size size_t]
      [screen_width unsigned-32]
      [screen_height unsigned-32]
      [cell_width unsigned-32]
      [cell_height unsigned-32]
      [padding_top unsigned-32]
      [padding_bottom unsigned-32]
      [padding_right unsigned-32]
      [padding_left unsigned-32]))
  (define-ftype GhosttyMousePosition (struct [x single-float] [y single-float]))
  (define-ftype GhosttySurfacePosition (struct [x double] [y double]))
  (define-ftype GhosttySelectionGestureBehaviors
    (struct [single_click int] [double_click int] [triple_click int]))
  (define-ftype GhosttySelectionGestureGeometry
    (struct
      [columns unsigned-32]
      [cell_width unsigned-32]
      [padding_left unsigned-32]
      [screen_height unsigned-32]))
  (define-ftype GhosttyWriter (struct [write uptr] [userdata uptr]))
  (define-ftype GhosttyMimeReader (struct [read uptr] [userdata uptr]))
  (define-ftype GhosttyPaste
    (struct
      [size size_t]
      [location int]
      [source int]
      [mimes uptr]
      [mimes_len size_t]
      [reader GhosttyMimeReader]
      [allow_unsafe stdbool]))
  (define-ftype GhosttySgrUnknown
    (struct [full_ptr uptr] [full_len size_t] [partial_ptr uptr] [partial_len size_t]))
  (define-ftype GhosttySgrAttributeValue
    (union
      [unknown GhosttySgrUnknown]
      [underline int]
      [underline_color GhosttyColorRgb]
      [underline_color_256 unsigned-8]
      [direct_color_fg GhosttyColorRgb]
      [direct_color_bg GhosttyColorRgb]
      [bg_8 unsigned-8]
      [fg_8 unsigned-8]
      [bright_bg_8 unsigned-8]
      [bright_fg_8 unsigned-8]
      [bg_256 unsigned-8]
      [fg_256 unsigned-8]
      [_padding (array 8 unsigned-64)]))
  (define-ftype GhosttySgrAttribute (struct [tag int] [value GhosttySgrAttributeValue]))
  (define-ftype GhosttyKittyGraphicsPlacementRenderInfo
    (struct
      [size size_t]
      [pixel_width unsigned-32]
      [pixel_height unsigned-32]
      [grid_cols unsigned-32]
      [grid_rows unsigned-32]
      [viewport_col integer-32]
      [viewport_row integer-32]
      [viewport_visible stdbool]
      [source_x unsigned-32]
      [source_y unsigned-32]
      [source_width unsigned-32]
      [source_height unsigned-32]))

  ;; A movable bytevector is safe only in calls that cannot run Scheme
  ;; code, so that no collection moves it while C reads it; calls that can
  ;; reach a callback take immobile bytevectors (c-struct with a true second
  ;; argument).
  (define-syntax c-struct
    (syntax-rules ()
      [(_ T) (make-bytevector (ftype-sizeof T) 0)]
      [(_ T immobile?)
       (if immobile?
           (make-immobile-bytevector (ftype-sizeof T) 0)
           (make-bytevector (ftype-sizeof T) 0))]))

  ;; A sized struct starts with its own size, which tells the library
  ;; which version of the struct the caller was compiled against.
  (define-syntax c-sized-struct
    (syntax-rules ()
      [(_ T . rest)
       (let ([bv (c-struct T . rest)])
         (bytevector-u64-native-set! bv 0 (ftype-sizeof T))
         bv)]))

  (define-syntax ftype-offset
    (syntax-rules ()
      [(_ T field ...)
       (ftype-pointer-address (ftype-&ref T (field ...) (make-ftype-pointer T 0)))]))

  (define-syntax c-ptr
    (syntax-rules ()
      [(_ bv) (make-ftype-scheme-object-pointer bv)]
      [(_ bv offset) (make-ftype-scheme-object-pointer bv offset)]))

  (define-syntax c-ref
    (syntax-rules ()
      [(_ T (field ...) bv) (ftype-any-ref T (field ...) (c-ptr bv))]
      [(_ T (field ...) bv offset) (ftype-any-ref T (field ...) (c-ptr bv offset))]))

  (define-syntax c-set!
    (syntax-rules ()
      [(_ T (field ...) bv value) (ftype-any-set! T (field ...) (c-ptr bv) value)]))

  (define memcpy-from (foreign-procedure "memcpy" (u8* uptr size_t) uptr))
  (define memcpy-to (foreign-procedure "memcpy" (uptr u8* size_t) uptr))

  (define (address->bytevector address len)
    (let ([bv (make-bytevector len)])
      (unless (fx= len 0) (memcpy-from bv address len))
      bv))

  (define ghostty-type-json (foreign-procedure "ghostty_type_json" () string))
  (define ghostty-build-info (foreign-procedure "ghostty_build_info" (int u8*) int))
  (define ghostty-free (foreign-procedure "ghostty_free" (uptr uptr size_t) void))

  (define ghostty-terminal-new
    (foreign-procedure "ghostty_terminal_new" (uptr u8* unsigned-16 unsigned-16) int))
  (define ghostty-terminal-free (foreign-procedure "ghostty_terminal_free" (uptr) void))
  (define ghostty-terminal-reset (foreign-procedure "ghostty_terminal_reset" (uptr) void))
  (define ghostty-terminal-resize
    (foreign-procedure "ghostty_terminal_resize"
      (uptr unsigned-16 unsigned-16 unsigned-32 unsigned-32) int))
  (define ghostty-terminal-set (foreign-procedure "ghostty_terminal_set" (uptr int uptr) int))
  (define ghostty-terminal-set-value (foreign-procedure "ghostty_terminal_set" (uptr int u8*) int))
  (define ghostty-terminal-vt-write
    (foreign-procedure "ghostty_terminal_vt_write" (uptr uptr size_t) void))
  (define ghostty-terminal-vt-write-until-ground
    (foreign-procedure "ghostty_terminal_vt_write_until_ground" (uptr uptr size_t u8*) int))
  (define ghostty-terminal-continuation-alloc
    (foreign-procedure "ghostty_terminal_continuation_alloc" (uptr uptr u8* u8*) int))
  (define ghostty-terminal-scroll-viewport
    (foreign-procedure "ghostty_terminal_scroll_viewport"
      (uptr (& GhosttyTerminalScrollViewport ftype-scheme-object-pointer)) void))
  (define ghostty-terminal-compression-activity
    (foreign-procedure "ghostty_terminal_compression_activity" (uptr u8*) int))
  (define ghostty-terminal-compress
    (foreign-procedure "ghostty_terminal_compress" (uptr int u8*) int))
  (define ghostty-terminal-get (foreign-procedure "ghostty_terminal_get" (uptr int u8*) int))
  (define ghostty-terminal-grid-ref
    (foreign-procedure "ghostty_terminal_grid_ref"
      (uptr (& GhosttyPoint ftype-scheme-object-pointer) u8*) int))
  (define ghostty-terminal-grid-ref-track
    (foreign-procedure "ghostty_terminal_grid_ref_track"
      (uptr (& GhosttyPoint ftype-scheme-object-pointer) u8*) int))
  (define ghostty-terminal-point-from-grid-ref
    (foreign-procedure "ghostty_terminal_point_from_grid_ref" (uptr u8* int u8*) int))

  (define ghostty-grid-ref-cell (foreign-procedure "ghostty_grid_ref_cell" (u8* u8*) int))
  (define ghostty-grid-ref-row (foreign-procedure "ghostty_grid_ref_row" (u8* u8*) int))
  (define ghostty-grid-ref-graphemes
    (foreign-procedure "ghostty_grid_ref_graphemes" (u8* u8* size_t u8*) int))
  (define ghostty-grid-ref-hyperlink-uri
    (foreign-procedure "ghostty_grid_ref_hyperlink_uri" (u8* u8* size_t u8*) int))
  (define ghostty-grid-ref-style (foreign-procedure "ghostty_grid_ref_style" (u8* u8*) int))

  (define ghostty-tracked-grid-ref-free
    (foreign-procedure "ghostty_tracked_grid_ref_free" (uptr) void))
  (define ghostty-tracked-grid-ref-has-value
    (foreign-procedure "ghostty_tracked_grid_ref_has_value" (uptr) stdbool))
  (define ghostty-tracked-grid-ref-point
    (foreign-procedure "ghostty_tracked_grid_ref_point" (uptr int u8*) int))
  (define ghostty-tracked-grid-ref-set
    (foreign-procedure "ghostty_tracked_grid_ref_set"
      (uptr uptr (& GhosttyPoint ftype-scheme-object-pointer)) int))
  (define ghostty-tracked-grid-ref-snapshot
    (foreign-procedure "ghostty_tracked_grid_ref_snapshot" (uptr u8*) int))

  (define ghostty-cell-get (foreign-procedure "ghostty_cell_get" (unsigned-64 int u8*) int))
  (define ghostty-row-get (foreign-procedure "ghostty_row_get" (unsigned-64 int u8*) int))

  (define ghostty-formatter-terminal-new
    (foreign-procedure "ghostty_formatter_terminal_new"
      (uptr u8* uptr (& GhosttyFormatterTerminalOptions ftype-scheme-object-pointer)) int))
  (define ghostty-formatter-format-alloc
    (foreign-procedure "ghostty_formatter_format_alloc" (uptr uptr u8* u8*) int))
  (define ghostty-formatter-free (foreign-procedure "ghostty_formatter_free" (uptr) void))

  (define ghostty-terminal-select-word
    (foreign-procedure "ghostty_terminal_select_word" (uptr u8* u8*) int))
  (define ghostty-terminal-select-word-between
    (foreign-procedure "ghostty_terminal_select_word_between" (uptr u8* u8*) int))
  (define ghostty-terminal-select-line
    (foreign-procedure "ghostty_terminal_select_line" (uptr u8* u8*) int))
  (define ghostty-terminal-select-all
    (foreign-procedure "ghostty_terminal_select_all" (uptr u8*) int))
  (define ghostty-terminal-select-output
    (foreign-procedure "ghostty_terminal_select_output"
      (uptr (& GhosttyGridRef ftype-scheme-object-pointer) u8*) int))
  (define ghostty-terminal-selection-format-alloc
    (foreign-procedure "ghostty_terminal_selection_format_alloc"
      (uptr uptr (& GhosttyTerminalSelectionFormatOptions ftype-scheme-object-pointer) u8* u8*) int))
  (define ghostty-terminal-selection-adjust
    (foreign-procedure "ghostty_terminal_selection_adjust" (uptr u8* int) int))
  (define ghostty-terminal-selection-order
    (foreign-procedure "ghostty_terminal_selection_order" (uptr u8* u8*) int))
  (define ghostty-terminal-selection-contains
    (foreign-procedure "ghostty_terminal_selection_contains"
      (uptr u8* (& GhosttyPoint ftype-scheme-object-pointer) u8*) int))

  (define ghostty-selection-gesture-new
    (foreign-procedure "ghostty_selection_gesture_new" (uptr u8*) int))
  (define ghostty-selection-gesture-free
    (foreign-procedure "ghostty_selection_gesture_free" (uptr uptr) void))
  (define ghostty-selection-gesture-reset
    (foreign-procedure "ghostty_selection_gesture_reset" (uptr uptr) void))
  (define ghostty-selection-gesture-get
    (foreign-procedure "ghostty_selection_gesture_get" (uptr uptr int u8*) int))
  (define ghostty-selection-gesture-event-new
    (foreign-procedure "ghostty_selection_gesture_event_new" (uptr u8* int) int))
  (define ghostty-selection-gesture-event-free
    (foreign-procedure "ghostty_selection_gesture_event_free" (uptr) void))
  (define ghostty-selection-gesture-event-set
    (foreign-procedure "ghostty_selection_gesture_event_set" (uptr int u8*) int))
  (define ghostty-selection-gesture-event
    (foreign-procedure "ghostty_selection_gesture_event" (uptr uptr uptr u8*) int))

  (define ghostty-search-new (foreign-procedure "ghostty_search_new" (uptr u8* uptr) int))
  (define ghostty-search-free (foreign-procedure "ghostty_search_free" (uptr) void))
  (define ghostty-search-tick (foreign-procedure "ghostty_search_tick" (uptr u8*) int))
  (define ghostty-search-feed (foreign-procedure "ghostty_search_feed" (uptr) int))
  (define ghostty-search-run (foreign-procedure "ghostty_search_run" (uptr) int))
  (define ghostty-search-set (foreign-procedure "ghostty_search_set" (uptr int u8*) int))
  (define ghostty-search-get (foreign-procedure "ghostty_search_get" (uptr int u8*) int))

  (define ghostty-render-state-new (foreign-procedure "ghostty_render_state_new" (uptr u8*) int))
  (define ghostty-render-state-free (foreign-procedure "ghostty_render_state_free" (uptr) void))
  (define ghostty-render-state-update
    (foreign-procedure "ghostty_render_state_update" (uptr uptr) int))
  (define ghostty-render-state-clean (foreign-procedure "ghostty_render_state_clean" (uptr) int))
  (define ghostty-render-state-get
    (foreign-procedure "ghostty_render_state_get" (uptr int u8*) int))
  (define ghostty-render-state-set
    (foreign-procedure "ghostty_render_state_set" (uptr int u8*) int))
  (define ghostty-render-state-row-iterator-new
    (foreign-procedure "ghostty_render_state_row_iterator_new" (uptr u8*) int))
  (define ghostty-render-state-row-iterator-free
    (foreign-procedure "ghostty_render_state_row_iterator_free" (uptr) void))
  (define ghostty-render-state-row-iterator-next
    (foreign-procedure "ghostty_render_state_row_iterator_next" (uptr) stdbool))
  (define ghostty-render-state-row-iterator-next-dirty
    (foreign-procedure "ghostty_render_state_row_iterator_next_dirty" (uptr u8*) stdbool))
  (define ghostty-render-state-row-get
    (foreign-procedure "ghostty_render_state_row_get" (uptr int u8*) int))
  (define ghostty-render-state-row-cells-new
    (foreign-procedure "ghostty_render_state_row_cells_new" (uptr u8*) int))
  (define ghostty-render-state-row-cells-free
    (foreign-procedure "ghostty_render_state_row_cells_free" (uptr) void))
  (define ghostty-render-state-row-cells-select
    (foreign-procedure "ghostty_render_state_row_cells_select" (uptr unsigned-16) int))
  (define ghostty-render-state-row-cells-get
    (foreign-procedure "ghostty_render_state_row_cells_get" (uptr int u8*) int))

  (define ghostty-snapshot-encode-alloc
    (foreign-procedure "ghostty_snapshot_encode_alloc" (uptr uptr u8* u8*) int))
  (define ghostty-snapshot-decoder-new-buf
    (foreign-procedure "ghostty_snapshot_decoder_new_buf" (uptr u8* uptr size_t) int))
  (define ghostty-snapshot-decoder-free
    (foreign-procedure "ghostty_snapshot_decoder_free" (uptr) void))
  (define ghostty-snapshot-decoder-set
    (foreign-procedure "ghostty_snapshot_decoder_set" (uptr int u8*) int))
  (define ghostty-snapshot-decoder-ready
    (foreign-procedure "ghostty_snapshot_decoder_ready" (uptr u8*) int))
  (define ghostty-snapshot-decoder-next
    (foreign-procedure "ghostty_snapshot_decoder_next" (uptr) int))
  (define ghostty-snapshot-decoder-decode
    (foreign-procedure "ghostty_snapshot_decoder_decode" (uptr u8*) int))
  (define ghostty-snapshot-decoder-get
    (foreign-procedure "ghostty_snapshot_decoder_get" (uptr int u8*) int))

  (define ghostty-key-encoder-new (foreign-procedure "ghostty_key_encoder_new" (uptr u8*) int))
  (define ghostty-key-encoder-free (foreign-procedure "ghostty_key_encoder_free" (uptr) void))
  (define ghostty-key-encoder-setopt
    (foreign-procedure "ghostty_key_encoder_setopt" (uptr int u8*) void))
  (define ghostty-key-encoder-setopt-from-terminal
    (foreign-procedure "ghostty_key_encoder_setopt_from_terminal" (uptr uptr) void))
  (define ghostty-key-encoder-encode
    (foreign-procedure "ghostty_key_encoder_encode" (uptr uptr u8* size_t u8*) int))
  (define ghostty-key-event-new (foreign-procedure "ghostty_key_event_new" (uptr u8*) int))
  (define ghostty-key-event-free (foreign-procedure "ghostty_key_event_free" (uptr) void))
  (define ghostty-key-event-set-action
    (foreign-procedure "ghostty_key_event_set_action" (uptr int) void))
  (define ghostty-key-event-set-key (foreign-procedure "ghostty_key_event_set_key" (uptr int) void))
  (define ghostty-key-event-set-mods
    (foreign-procedure "ghostty_key_event_set_mods" (uptr unsigned-16) void))
  (define ghostty-key-event-set-consumed-mods
    (foreign-procedure "ghostty_key_event_set_consumed_mods" (uptr unsigned-16) void))
  (define ghostty-key-event-set-composing
    (foreign-procedure "ghostty_key_event_set_composing" (uptr stdbool) void))
  (define ghostty-key-event-set-utf8
    (foreign-procedure "ghostty_key_event_set_utf8" (uptr uptr size_t) void))
  (define ghostty-key-event-set-unshifted-codepoint
    (foreign-procedure "ghostty_key_event_set_unshifted_codepoint" (uptr unsigned-32) void))

  (define ghostty-mouse-encoder-new (foreign-procedure "ghostty_mouse_encoder_new" (uptr u8*) int))
  (define ghostty-mouse-encoder-free (foreign-procedure "ghostty_mouse_encoder_free" (uptr) void))
  (define ghostty-mouse-encoder-setopt
    (foreign-procedure "ghostty_mouse_encoder_setopt" (uptr int u8*) void))
  (define ghostty-mouse-encoder-setopt-from-terminal
    (foreign-procedure "ghostty_mouse_encoder_setopt_from_terminal" (uptr uptr) void))
  (define ghostty-mouse-encoder-reset (foreign-procedure "ghostty_mouse_encoder_reset" (uptr) void))
  (define ghostty-mouse-encoder-encode
    (foreign-procedure "ghostty_mouse_encoder_encode" (uptr uptr u8* size_t u8*) int))
  (define ghostty-mouse-event-new (foreign-procedure "ghostty_mouse_event_new" (uptr u8*) int))
  (define ghostty-mouse-event-free (foreign-procedure "ghostty_mouse_event_free" (uptr) void))
  (define ghostty-mouse-event-set-action
    (foreign-procedure "ghostty_mouse_event_set_action" (uptr int) void))
  (define ghostty-mouse-event-set-button
    (foreign-procedure "ghostty_mouse_event_set_button" (uptr int) void))
  (define ghostty-mouse-event-clear-button
    (foreign-procedure "ghostty_mouse_event_clear_button" (uptr) void))
  (define ghostty-mouse-event-set-mods
    (foreign-procedure "ghostty_mouse_event_set_mods" (uptr unsigned-16) void))
  (define ghostty-mouse-event-set-position
    (foreign-procedure "ghostty_mouse_event_set_position"
      (uptr (& GhosttyMousePosition ftype-scheme-object-pointer)) void))

  (define ghostty-osc-new (foreign-procedure "ghostty_osc_new" (uptr u8*) int))
  (define ghostty-osc-free (foreign-procedure "ghostty_osc_free" (uptr) void))
  (define ghostty-osc-set (foreign-procedure "ghostty_osc_set" (uptr int u8*) int))
  (define ghostty-osc-next (foreign-procedure "ghostty_osc_next" (uptr unsigned-8) void))
  (define ghostty-osc-end (foreign-procedure "ghostty_osc_end" (uptr unsigned-8) uptr))
  (define ghostty-osc-command-type (foreign-procedure "ghostty_osc_command_type" (uptr) int))
  (define ghostty-osc-command-data
    (foreign-procedure "ghostty_osc_command_data" (uptr int u8*) stdbool))

  (define ghostty-sgr-new (foreign-procedure "ghostty_sgr_new" (uptr u8*) int))
  (define ghostty-sgr-free (foreign-procedure "ghostty_sgr_free" (uptr) void))
  (define ghostty-sgr-set-params
    (foreign-procedure "ghostty_sgr_set_params" (uptr u8* u8* size_t) int))
  (define ghostty-sgr-next (foreign-procedure "ghostty_sgr_next" (uptr u8*) stdbool))

  (define ghostty-color-parse (foreign-procedure "ghostty_color_parse" (u8* size_t u8*) int))
  (define ghostty-color-parse-x11
    (foreign-procedure "ghostty_color_parse_x11" (u8* size_t u8*) int))
  (define ghostty-color-parse-palette-entry
    (foreign-procedure "ghostty_color_parse_palette_entry" (u8* size_t u8* u8*) int))
  (define ghostty-color-palette-default
    (foreign-procedure "ghostty_color_palette_default" (u8*) void))
  (define ghostty-color-palette-generate
    (foreign-procedure "ghostty_color_palette_generate" (u8* u8* u8* u8* stdbool u8*) void))
  (define ghostty-color-luminance (foreign-procedure "ghostty_color_luminance" (u8*) double))
  (define ghostty-color-perceived-luminance
    (foreign-procedure "ghostty_color_perceived_luminance" (u8*) double))
  (define ghostty-color-contrast (foreign-procedure "ghostty_color_contrast" (u8* u8*) double))
  (define ghostty-color-x11-names (foreign-procedure "ghostty_color_x11_names" () uptr))
  (define ghostty-color-x11-name-count (foreign-procedure "ghostty_color_x11_name_count" () size_t))

  (define ghostty-color-scheme-report-encode
    (foreign-procedure "ghostty_color_scheme_report_encode" (int u8* size_t u8*) int))
  (define ghostty-focus-encode (foreign-procedure "ghostty_focus_encode" (int u8* size_t u8*) int))
  (define ghostty-mode-report-encode
    (foreign-procedure "ghostty_mode_report_encode" (unsigned-16 int u8* size_t u8*) int))
  (define ghostty-size-report-encode
    (foreign-procedure "ghostty_size_report_encode"
      (int (& GhosttySizeReportSize ftype-scheme-object-pointer) u8* size_t u8*) int))
  (define ghostty-paste-is-safe (foreign-procedure "ghostty_paste_is_safe" (u8* size_t) stdbool))
  (define ghostty-paste-encode
    (foreign-procedure "ghostty_paste_encode" (u8* size_t stdbool u8* size_t u8*) int))
  (define ghostty-terminal-paste (foreign-procedure "ghostty_terminal_paste" (uptr uptr uptr) int))

  (define ghostty-unicode-codepoint-width
    (foreign-procedure "ghostty_unicode_codepoint_width" (unsigned-32) unsigned-8))
  (define ghostty-unicode-grapheme-width
    (foreign-procedure "ghostty_unicode_grapheme_width" (u8* size_t u8*) size_t))

  (define ghostty-kitty-graphics-get
    (foreign-procedure "ghostty_kitty_graphics_get" (uptr int u8*) int))
  (define ghostty-kitty-graphics-image
    (foreign-procedure "ghostty_kitty_graphics_image" (uptr unsigned-32) uptr))
  (define ghostty-kitty-graphics-image-get
    (foreign-procedure "ghostty_kitty_graphics_image_get" (uptr int u8*) int))
  (define ghostty-kitty-graphics-placement-iterator-new
    (foreign-procedure "ghostty_kitty_graphics_placement_iterator_new" (uptr u8*) int))
  (define ghostty-kitty-graphics-placement-iterator-free
    (foreign-procedure "ghostty_kitty_graphics_placement_iterator_free" (uptr) void))
  (define ghostty-kitty-graphics-placement-next
    (foreign-procedure "ghostty_kitty_graphics_placement_next" (uptr) stdbool))
  (define ghostty-kitty-graphics-placement-get
    (foreign-procedure "ghostty_kitty_graphics_placement_get" (uptr int u8*) int))
  (define ghostty-kitty-graphics-placement-render-info
    (foreign-procedure "ghostty_kitty_graphics_placement_render_info" (uptr uptr uptr u8*) int)))
