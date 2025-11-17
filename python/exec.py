import sys

# Don't use 'for line in sys.stdin' because it buffers input
while True:
    line = sys.stdin.readline()
    if not line:
        break
    # `eval` to parse string into code, `exec` to execute the code
    exec(eval(line))
    sys.stdout.flush()
