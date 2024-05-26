local M = {};

function M.setup(opts)
    vim.filetype.add({
        extension = {
            gek = "geckscript-nvse",
            geck = "geckscript-nvse",
        }
    })

    local util = require("lspconfig.util")
    local cmd = { "nvse-lsp", "--stdio" }

    local default_config = {
        default_config = {
            cmd = cmd,
            filetypes = { "geckscript-nvse" },
            root_dir = util.find_git_ancestor,
            single_file_support = true,
        },
        docs = {
            description = [[
    https://github.com/WarZone762/nvse-lsp
    ]],
            default_config = {
                root_dir = [[util.find_git_ancestor]],
            },
        },
    }

    local configs = require("lspconfig.configs")
    configs["nvse-lsp"] = default_config
    configs["nvse-lsp"].setup(opts)
end

return M
