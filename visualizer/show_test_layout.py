#!/usr/bin/env python3
import re
import sys
from xml.etree.ElementTree import Element, SubElement, tostring


def generate_svg(input_string):
    svg = Element("svg", {"xmlns": "http://www.w3.org/2000/svg"})

    max_x = 0
    max_y = 0

    pattern = re.compile(r"x: (\d+),\s*y: (\d+),\s*width: (\d+),\s*height: (\d+)", re.M)

    for i, match in enumerate(pattern.finditer(input_string)):
        x, y, width, height = map(int, match.groups())

        max_x = max(max_x, x + width)
        max_y = max(max_y, y + height)

        rect = SubElement(svg, "rect")
        rect.set("x", str(x))
        rect.set("y", str(y))
        rect.set("width", str(width))
        rect.set("height", str(height))

        text = SubElement(svg, "text")
        text.set("x", str(x + 2))
        text.set("y", str(y + 20))
        text.set("stroke", "none")
        text.set("fill", "white")
        text.set("font-size", "20")
        text.text = str(i)

    svg.set("width", str(max_x))
    svg.set("height", str(max_y))

    svg_str = tostring(svg)

    return svg_str


if __name__ == "__main__":
    input_string = sys.stdin.read().strip()

    svg_content = generate_svg(input_string)

    with open("test_layout.svg", "wb") as f:
        f.write(svg_content)
