(library (ghostty)
  (export
    &ghostty ghostty-error? ghostty-error-result ghostty-error-code

    make-rgb rgb? rgb-red rgb-green rgb-blue
    style? style-foreground style-background style-underline-color style-bold? style-italic?
    style-faint? style-blink? style-inverse? style-invisible? style-strikethrough?
    style-overline? style-underline

    terminal? make-terminal terminal-close! terminal-closed?
    terminal-write! terminal-write-until-ground! terminal-reset! terminal-resize!
    terminal-paste! terminal-paste-unsafe!

    terminal-columns terminal-rows terminal-pixel-width terminal-pixel-height
    terminal-total-rows terminal-scrollback-rows terminal-title terminal-pwd
    terminal-mouse-tracking? terminal-mouse-shape terminal-viewport-active? terminal-vt-ground?
    terminal-vt-processing-error? terminal-active-screen
    cursor? cursor-x cursor-y cursor-pending-wrap? cursor-visible? cursor-at-prompt?
    terminal-cursor terminal-cursor-pen
    scrollbar? scrollbar-total scrollbar-offset scrollbar-length terminal-scrollbar
    color-theme? color-theme-foreground color-theme-background color-theme-cursor
    color-theme-palette terminal-colors terminal-default-colors
    kitty-key-flag kitty-key-flags terminal-kitty-keyboard-flags
    memory-usage? memory-usage-compression-supported? memory-usage-primary-pages
    memory-usage-primary-virtual-bytes memory-usage-primary-resident-bytes
    memory-usage-primary-compressed-pages memory-usage-primary-compressed-bytes
    memory-usage-primary-image-bytes memory-usage-alternate-pages
    memory-usage-alternate-virtual-bytes memory-usage-alternate-resident-bytes
    memory-usage-alternate-compressed-pages memory-usage-alternate-compressed-bytes
    memory-usage-alternate-image-bytes terminal-memory-usage
    terminal-continuation-max-bytes terminal-scrollback-max-bytes terminal-scrollback-max-lines
    terminal-clipboard-write-max-bytes terminal-kitty-image-storage-limit
    terminal-kitty-image-file-medium? terminal-kitty-image-shared-memory-medium?
    terminal-kitty-image-temp-directory

    terminal-modes terminal-mode-enabled? terminal-set-mode! terminal-set-mode-default!
    terminal-set-title! terminal-set-pwd! terminal-set-terminfo-name!
    terminal-set-default-foreground! terminal-set-default-background!
    terminal-set-default-cursor-color! terminal-set-default-palette!
    terminal-set-scrollback-max-bytes! terminal-set-scrollback-max-lines!
    terminal-set-continuation-max-bytes! terminal-set-apc-max-bytes!
    terminal-set-kitty-graphics-max-bytes! terminal-set-unknown-max-bytes!
    terminal-set-clipboard-write-max-bytes! terminal-set-kitty-image-storage-limit!
    terminal-set-kitty-image-file-medium! terminal-set-kitty-image-shared-memory-medium!
    terminal-set-kitty-image-temp-directory! terminal-set-default-cursor-style!
    terminal-set-default-cursor-blink! terminal-set-glyph-protocol! terminal-set-title-report!
    terminal-set-resize-pull-scrollback! terminal-set-checksum-report!
    terminal-set-checksum-extension!

    terminal-scroll-to-top! terminal-scroll-to-bottom! terminal-scroll-by!
    terminal-scroll-to-row! terminal-scroll-by-prompts!
    terminal-compress! terminal-compress-fully! terminal-compression-activity

    format-option format-options terminal-format terminal->string terminal-continuation

    terminal-set-pty-write-handler! terminal-set-bell-handler! terminal-set-enquiry-handler!
    terminal-set-xtversion-handler! terminal-set-title-changed-handler!
    terminal-set-pwd-changed-handler! terminal-set-size-handler!
    terminal-set-color-scheme-handler! terminal-set-device-attributes-handler!
    terminal-set-clipboard-write-handler! terminal-set-clipboard-read-handler!
    terminal-set-desktop-notification-handler! terminal-set-progress-report-handler!
    terminal-set-unknown-sequence-handler! terminal-set-semantic-prompt-handler!
    terminal-set-program-status-handler! terminal-set-resize-window-handler!
    terminal-set-reset-handler! terminal-set-render-hold-handler!
    make-clipboard-content clipboard-content? clipboard-content-mime clipboard-content-data
    clipboard-write-request? clipboard-write-request-location clipboard-write-request-contents
    clipboard-write-request-name clipboard-write-request-granted?
    clipboard-write-request-can-remember?
    clipboard-read-request? clipboard-read-request-location clipboard-read-request-mimes
    clipboard-read-request-list? clipboard-read-request-name clipboard-read-request-granted?
    clipboard-read-request-can-remember?
    desktop-notification? desktop-notification-title desktop-notification-body
    progress-report? progress-report-state progress-report-progress
    unknown-sequence? unknown-sequence-kind unknown-sequence-content
    unknown-sequence-truncated? unknown-sequence-terminator
    semantic-prompt? semantic-prompt-kind semantic-prompt-prompt-kind
    semantic-prompt-exit-code semantic-prompt-command semantic-prompt-error
    program-status? program-status-state program-status-kind program-status-progress
    program-status-id program-status-app program-status-title program-status-message
    make-terminal-size terminal-size? terminal-size-rows terminal-size-columns
    terminal-size-cell-width terminal-size-cell-height
    make-device-attributes device-attributes? device-attributes-conformance-level
    device-attributes-features device-attributes-device-type
    device-attributes-firmware-version device-attributes-rom-cartridge
    device-attributes-unit-id

    make-grid-point grid-point? grid-point-space grid-point-x grid-point-y
    terminal-point-convert terminal-cell terminal-row
    grid-cell? grid-cell-codepoint grid-cell-text grid-cell-wide grid-cell-content
    grid-cell-has-text? grid-cell-has-styling? grid-cell-style-id grid-cell-style
    grid-cell-hyperlink grid-cell-protected? grid-cell-semantic-content grid-cell-background
    grid-row? grid-row-wrap? grid-row-wrap-continuation? grid-row-grapheme? grid-row-styled?
    grid-row-hyperlink? grid-row-semantic-prompt grid-row-kitty-virtual-placeholder?
    grid-row-dirty?
    tracked-point? make-tracked-point tracked-point-close! tracked-point-closed?
    tracked-point-valid? tracked-point-ref tracked-point-move! tracked-point-cell
    tracked-point-row

    selection? selection-start selection-end selection-rectangle?
    terminal-selection terminal-select! terminal-select-rectangle! terminal-clear-selection!
    terminal-select-all! terminal-select-word! terminal-select-word-between!
    terminal-select-line! terminal-select-semantic-line! terminal-select-output!
    terminal-selection-adjust!
    terminal-selection-order terminal-selection-contains? terminal-format-selection
    terminal-selection->string

    search? make-search search-close! search-closed? search-set-needle! search-status
    search-feed! search-tick! search-run! search-total search-selected-index
    search-selected-match search-matches search-viewport-matches search-select-next!
    search-select-previous! search-set-scroll!

    selection-gesture? make-selection-gesture selection-gesture-close!
    selection-gesture-closed? selection-gesture-set-behaviors!
    selection-gesture-set-word-boundaries! selection-gesture-set-repeat-limits!
    make-gesture-geometry gesture-geometry? gesture-geometry-columns
    gesture-geometry-cell-width gesture-geometry-padding-left gesture-geometry-screen-height
    selection-gesture-press! selection-gesture-drag! selection-gesture-drag-rectangle!
    selection-gesture-autoscroll-tick! selection-gesture-autoscroll-tick-rectangle!
    selection-gesture-release! selection-gesture-deep-press! selection-gesture-reset!
    gesture-state? gesture-state-click-count gesture-state-dragged? gesture-state-autoscroll
    gesture-state-behavior gesture-state-anchor selection-gesture-state

    render-state? make-render-state render-state-close! render-state-closed?
    render-state-update! render-state-dirty render-state-columns render-state-row-count
    render-state-clean! render-state-set-overscan! render-state-overscan
    render-colors? render-colors-background render-colors-foreground render-colors-cursor
    render-colors-palette render-state-colors
    render-cursor? render-cursor-style render-cursor-visible? render-cursor-blinking?
    render-cursor-password-input? render-cursor-column render-cursor-row
    render-cursor-wide-tail? render-state-cursor
    render-row? render-row-y render-row-id render-row-dirty? render-row-wrap?
    render-row-wrap-continuation? render-row-semantic-prompt render-row-selection-start
    render-row-selection-end render-row-runs
    render-run? render-run-column render-run-width render-run-text render-run-style
    render-run-selected? render-run-hyperlink render-run-semantic-content
    render-state-rows render-state-dirty-rows

    key-modifier key-modifiers
    make-key-event key-event? key-event-action key-event-key key-event-modifiers
    key-event-text key-event-unshifted key-event-composing? key-event-consumed-modifiers
    make-mouse-event mouse-event? mouse-event-action mouse-event-button mouse-event-x
    mouse-event-y mouse-event-modifiers
    make-mouse-geometry mouse-geometry? mouse-geometry-screen-width mouse-geometry-screen-height
    mouse-geometry-cell-width mouse-geometry-cell-height mouse-geometry-padding-top
    mouse-geometry-padding-bottom mouse-geometry-padding-right mouse-geometry-padding-left
    key-encoder? make-key-encoder key-encoder-close! key-encoder-closed? key-encoder-sync!
    key-encoder-set-cursor-key-application! key-encoder-set-keypad-key-application!
    key-encoder-set-ignore-keypad-with-numlock! key-encoder-set-alt-esc-prefix!
    key-encoder-set-modify-other-keys! key-encoder-set-backarrow-key-mode!
    key-encoder-set-kitty-flags! key-encoder-set-option-as-alt! key-encoder-encode
    mouse-encoder? make-mouse-encoder mouse-encoder-close! mouse-encoder-closed?
    mouse-encoder-sync! mouse-encoder-set-tracking! mouse-encoder-set-format!
    mouse-encoder-set-geometry! mouse-encoder-set-any-button-pressed!
    mouse-encoder-set-track-last-cell! mouse-encoder-reset! mouse-encoder-encode

    terminal->snapshot snapshot->terminal
    snapshot-decoder? make-snapshot-decoder snapshot-decoder-ready! snapshot-decoder-next!
    snapshot-decoder-option snapshot-decoder-options snapshot-decoder-source-offset
    snapshot-decoder-history snapshot-decoder-close! snapshot-decoder-closed?
    snapshot-progress? snapshot-progress-screen snapshot-progress-rows
    snapshot-progress-remaining

    terminal-kitty-generation terminal-kitty-placements terminal-kitty-image
    kitty-placement? kitty-placement-image-id kitty-placement-placement-id
    kitty-placement-virtual? kitty-placement-x-offset kitty-placement-y-offset
    kitty-placement-z kitty-placement-layer kitty-placement-pixel-width
    kitty-placement-pixel-height kitty-placement-columns kitty-placement-rows
    kitty-placement-viewport-column kitty-placement-viewport-row kitty-placement-source-x
    kitty-placement-source-y kitty-placement-source-width kitty-placement-source-height
    kitty-image? kitty-image-id kitty-image-number kitty-image-width kitty-image-height
    kitty-image-format kitty-image-generation kitty-image-data

    libghostty-build-info build-info? build-info-simd? build-info-kitty-graphics?
    build-info-tmux-control-mode? build-info-optimize build-info-version build-info-major
    build-info-minor build-info-patch build-info-pre build-info-build
    color-parse color-parse-x11 color-parse-palette-entry color-default-palette
    color-generate-palette color-generate-harmonious-palette color-luminance
    color-perceived-luminance color-contrast
    color-x11-colors
    color-scheme-report-encode focus-encode mode-report-encode size-report-encode
    paste-safe? paste-encode paste-encode-bracketed
    unicode-codepoint-width unicode-grapheme-width
    osc-parse osc-command? osc-command-kind osc-command-data
    sgr-parse sgr-attribute? sgr-attribute-kind sgr-attribute-value
    sgr-unknown? sgr-unknown-full sgr-unknown-partial)
  (import (chezscheme) (ghostty ffi))

  ;; Chez finds these files, and build/config.ss included by (ghostty ffi),
  ;; only through source-directories, even when loading compiled code: a
  ;; program importing (ghostty) needs this library's src folder and root
  ;; there.
  (include "ghostty/core.ss")
  (include "ghostty/terminal.ss")
  (include "ghostty/effects.ss")
  (include "ghostty/grid.ss")
  (include "ghostty/search.ss")
  (include "ghostty/render.ss")
  (include "ghostty/input.ss")
  (include "ghostty/utilities.ss")
  (include "ghostty/snapshot.ss")
  (include "ghostty/kitty.ss"))
