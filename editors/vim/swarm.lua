if vim.fn.executable('swarm') == 1 then
    vim.lsp.start({
        name = 'Swarm Language Server',
        cmd = { 'swarm', 'lsp' },
    })
end


