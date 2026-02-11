nvim +'set rtp+=./nvse-lsp.nvim | lua require("nvse-lsp").setup({cmd = {"./target/debug/nvse-lsp"}})' .
