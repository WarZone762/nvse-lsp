local vim = vim
local api = vim.api

local tree_buf = -1
local cur_buf = -1
local ns = -1
local augroup = api.nvim_create_augroup("nvse-lsp/inspect", {})

local function node_range()
    local w = api.nvim_get_current_win()
    local row = api.nvim_win_get_cursor(w)[1]

    local str = api.nvim_buf_get_lines(tree_buf, row - 1, row, false)[1];
    local iter = string.gmatch(str, "%d+")
    local s, e = tonumber(iter()) + 1, tonumber(iter())

    local lnum, col, end_lnum, end_col = 0, 0, 0, 0

    api.nvim_buf_call(cur_buf, function()
        lnum = vim.fn.byte2line(s)
        col = s - vim.fn.line2byte(lnum)
        end_lnum = vim.fn.byte2line(e)
        end_col = e - vim.fn.line2byte(end_lnum)
    end)

    return lnum - 1, col, end_lnum - 1, end_col + 1
end

local function set_ast(uri, ast)
    cur_buf = vim.uri_to_bufnr(uri)
    if tree_buf == -1 then return end
    api.nvim_create_autocmd("CursorMoved", {
        group = augroup,
        buffer = tree_buf,
        callback = function()
            if not api.nvim_buf_is_loaded(tree_buf) then
                return true
            end

            api.nvim_buf_clear_namespace(cur_buf, ns, 0, -1)
            local lnum, col, end_lnum, end_col = node_range()
            api.nvim_buf_set_extmark(cur_buf, ns, lnum, col, {
                end_row = end_lnum,
                end_col = math.max(0, end_col),
                hl_group = "Visual",
                strict = false,
            })
        end,
    })
    if tree_buf == -1 then return end
    api.nvim_buf_set_option(tree_buf, "modifiable", true)
    api.nvim_buf_set_lines(tree_buf, 0, -1, true, {})
    api.nvim_buf_set_lines(tree_buf, 0, -1, true, vim.split(ast, "\n"))
    api.nvim_buf_set_option(tree_buf, "modifiable", false)
    api.nvim_buf_set_option(tree_buf, "modified", false)
end



local function open_buf()
    local visible = api.nvim_call_function("bufwinnr", { tree_buf }) ~= -1

    if tree_buf ~= -1 and visible then return end

    ns = api.nvim_create_namespace("nvse-lsp")
    cur_buf = api.nvim_get_current_buf()
    api.nvim_command("botright vsplit nvse_lsp_ast_view")
    tree_buf = api.nvim_get_current_buf()
    vim.opt_local.modifiable = false
    api.nvim_create_autocmd("CursorMoved", {
        group = augroup,
        buffer = tree_buf,
        callback = function()
            if not api.nvim_buf_is_loaded(tree_buf) then
                return true
            end

            local w = api.nvim_get_current_win()
            api.nvim_buf_clear_namespace(cur_buf, ns, 0, -1)
            local lnum, col, end_lnum, end_col = node_range()
            api.nvim_buf_set_extmark(cur_buf, ns, lnum, col, {
                end_row = end_lnum,
                end_col = math.max(0, end_col),
                hl_group = "Visual",
                strict = false,
            })
        end,
    })

    api.nvim_command("wincmd p")
end

local M = {};

function M.setup(opts)
    vim.filetype.add({
        extension = {
            gek = "nvsescript",
            geck = "nvsescript",
        }
    })

    local util = require("lspconfig.util")
    local cmd = { "nvse-lsp", "--stdio" }

    local default_config = {
        default_config = {
            cmd = cmd,
            filetypes = { "nvsescript" },
            root_dir = util.find_git_ancestor,
            single_file_support = true,
            handlers = {
                ["nvse-lsp/ast"] = function(err, result, ctx, config)
                    set_ast(result.uri, result.ast)
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

    api.nvim_create_user_command("NvseInspect", function()
        open_buf()
    end, {})
end

return M
