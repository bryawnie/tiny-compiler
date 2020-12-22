rbp = 0
rsp = 0
rax = 0


def alloc_memory(size):
    global mem
    mem = [None] * size

def memread(memory_address):
    global mem
    return mem[memory_address // 8]

def memwrite(memory_address, value):
    global mem
    mem[memory_address // 8] = value

def push(val):
    global rsp
    memwrite(rsp, val)
    rsp -= 8

def pop():
    global rsp
    val = memread(rsp)
    rsp += 8
    return val
