#!/usr/bin/env python3

"""Assemble verified Figure S5 PNG panels into a six-page PDF."""

from pathlib import Path
import sys

from PIL import Image
from reportlab.lib.utils import ImageReader
from reportlab.pdfgen import canvas


PAGE_WIDTH = 13.5 * 72
PAGE_HEIGHT = 8.5 * 72


def main() -> None:
    if len(sys.argv) != 8:
        raise SystemExit(
            "Usage: assemble_figure_s05_pdf.py OUTPUT.pdf PANEL_A.png ... PANEL_F.png"
        )

    output_path = Path(sys.argv[1])
    panel_paths = [Path(path) for path in sys.argv[2:]]
    missing = [str(path) for path in panel_paths if not path.exists()]
    if missing:
        raise FileNotFoundError(f"Missing Figure S5 panels: {', '.join(missing)}")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    document = canvas.Canvas(
        str(output_path),
        pagesize=(PAGE_WIDTH, PAGE_HEIGHT),
        pageCompression=1,
    )

    for panel_path in panel_paths:
        with Image.open(panel_path) as image:
            image_width, image_height = image.size
        scale = min(PAGE_WIDTH / image_width, PAGE_HEIGHT / image_height)
        draw_width = image_width * scale
        draw_height = image_height * scale
        x_position = (PAGE_WIDTH - draw_width) / 2
        y_position = (PAGE_HEIGHT - draw_height) / 2
        document.drawImage(
            ImageReader(str(panel_path)),
            x_position,
            y_position,
            width=draw_width,
            height=draw_height,
            preserveAspectRatio=True,
            mask="auto",
        )
        document.showPage()

    document.save()


if __name__ == "__main__":
    main()
