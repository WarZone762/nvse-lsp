SRCS := $(shell find src -name '*.rs')

run: run-debug

run-debug: target/debug/nvse-lsp
	nvim +'set rtp+=./nvse-lsp.nvim | lua require("nvse-lsp").setup({cmd = {"./target/debug/nvse-lsp"}})' .

run-release: target/release/nvse-lsp
	nvim +'set rtp+=./nvse-lsp.nvim | lua require("nvse-lsp").setup({cmd = {"./target/release/nvse-lsp"}})' .

target/debug/nvse-lsp: ${SRCS}
	cargo build

target/release/nvse-lsp: ${SRCS}
	cargo build --release
