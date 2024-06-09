tydef D = rec d. d -> d end

def selfApp : D -> D = \x. x x end

def omega : D = selfApp selfApp end
