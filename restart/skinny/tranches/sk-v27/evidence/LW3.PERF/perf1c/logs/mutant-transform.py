import sys
s=sys.stdin.read()
sys.stdout.write(s+"\n/// mutant: a second copy\n#[allow(dead_code)]\npub enum KernelShape { NibbleLut }\n")
