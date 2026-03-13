SRCS := $(shell find src -name '*.rs')
TARGET_DEBUG = target/debug/nvse-lsp
TARGET_RELEASE = target/release/nvse-lsp

run: run-debug

run-debug: ${TARGET_DEBUG}
	nvim +'set rtp+=./nvse-lsp.nvim | lua require("nvse-lsp").setup({cmd = {"./${TARGET_DEBUG}"}})' .

run-release: ${TARGET_RELEASE}
	nvim +'set rtp+=./nvse-lsp.nvim | lua require("nvse-lsp").setup({cmd = {"./${TARGET_RELEASE}"}})' .

${TARGET_DEBUG}: ${SRCS}
	cargo build

${TARGET_RELEASE}: ${SRCS}
	cargo build --release

generate-ast: ${TARGET_RELEASE}
	scripts/generate-ast.sh
