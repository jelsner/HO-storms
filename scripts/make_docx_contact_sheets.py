from pathlib import Path
import re
from PIL import Image, ImageDraw

source = Path("/private/tmp/ho-storms-table1-render")
pages = sorted(
    source.glob("page-*.png"),
    key=lambda path: int(re.search(r"(\d+)$", path.stem).group(1)),
)

columns, rows = 3, 4
thumb_width = 400
label_height = 28
for batch_start in range(0, len(pages), columns * rows):
    batch = pages[batch_start : batch_start + columns * rows]
    first = Image.open(batch[0]).convert("RGB")
    thumb_height = round(first.height * thumb_width / first.width)
    sheet = Image.new(
        "RGB",
        (columns * thumb_width, rows * (thumb_height + label_height)),
        "white",
    )
    draw = ImageDraw.Draw(sheet)
    for index, path in enumerate(batch):
        image = Image.open(path).convert("RGB")
        image.thumbnail((thumb_width, thumb_height))
        column = index % columns
        row = index // columns
        x = column * thumb_width
        y = row * (thumb_height + label_height)
        sheet.paste(image, (x, y + label_height))
        draw.text((x + 8, y + 6), path.stem, fill="black")
    output = source / f"contact-{batch_start // (columns * rows) + 1}.png"
    sheet.save(output)
    print(output)
