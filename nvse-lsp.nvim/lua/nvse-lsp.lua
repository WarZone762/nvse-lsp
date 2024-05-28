local buf_nr = -1

local function set_ast(ast)
    if buf_nr == -1 then return end
    vim.api.nvim_buf_set_option(buf_nr, "modifiable", true)
    vim.api.nvim_buf_set_lines(buf_nr, 0, -1, true, {})
    vim.api.nvim_buf_set_lines(buf_nr, 0, -1, true, vim.split(ast, "\n"))
    vim.api.nvim_buf_set_option(buf_nr, "modifiable", false)
    vim.api.nvim_buf_set_option(buf_nr, "modified", false)
end



local function open_buf()
    local visible = vim.api.nvim_call_function("bufwinnr", { buf_nr }) ~= -1

    if buf_nr ~= -1 and visible then return end

    vim.api.nvim_command("botright vsplit nvse_lsp_ast_view")
    buf_nr = vim.api.nvim_get_current_buf()
    vim.opt_local.modifiable = false
    vim.api.nvim_command("wincmd p")
end

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
            handlers = {
                ["geckscript-nvse/ast"] = function(err, result, ctx, config)
                    set_ast(result)
                end
            }
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

    vim.api.nvim_create_user_command("NvseInspect", function()
        open_buf()
    end, {})
end

return M
