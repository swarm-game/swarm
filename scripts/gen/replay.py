#!/usr/bin/env python3
import json
import io
import sys
from typing import Union, TextIO

# Sample JSON input
sample_history = [
  {
    "in": "move",
    "tick": 84
  },
  {
    "in": "turn right",
    "tick": 148
  }
]

def process(history: list[dict]) -> list[str]:
    # Generate output with wait commands
    output = []
    previous_tick = 0

    for cmd in history:
        if 'in' not in cmd:
            continue  # output or error
        wait_time = cmd["tick"] - previous_tick
        if wait_time > 0:
            output.append(f"wait {wait_time};")
        output.append(cmd["in"] + ";")
        previous_tick = cmd["tick"]
    
    return output
    

def parse(input: Union[str|TextIO]) -> list[dict]:
    if isinstance(input, io.TextIOBase):  # file-like object
        return json.load(input)
    elif isinstance(input, str):  # JSON string
        return json.loads(input)
    else:
        raise Exception(f"Can not parse '{type(input)}'")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Process command JSON with wait times.")
    parser.add_argument("file", nargs="?", help="Path to input JSON file. Reads from stdin if omitted.")
    args = parser.parse_args()

    if args.file:
        with open(args.file, "r") as f:
            input_file = f.read()
    else:
        input_file = sys.stdin.read()

    json_data = parse(input_file)
    result = process(json_data)

    # Print each command on a new line
    print("\n".join(result))
