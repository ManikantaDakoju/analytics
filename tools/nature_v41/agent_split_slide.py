"""One-slide pitch for the Nature Opportunity Agent, split view: left = how it works (with EY review points),
right = what it brings to the client (each benefit tagged with the steps that deliver it). EY format of the master deck.
Usage: python agent_split_slide.py MASTER.pptx ICON_DIR OUT.pptx   (icons: Lucide, ISC licence)"""
import sys, copy
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.enum.shapes import MSO_SHAPE, MSO_CONNECTOR
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
MASTER, ICONS, OUT = sys.argv[1:4]
Y, DARK, WH, GR, CARD, LINE = 'FFE600', '1A1A24', 'F2F2F2', 'BFBFC7', '2E2E38', '3A3A46'
B, L = 'EYInterstate', 'EYInterstate Light'
p = Presentation(MASTER); src = p.slides[1]
s = p.slides.add_slide(src.slide_layout)
for sp in list(s.shapes): sp._element.getparent().remove(sp._element)
def shp(name): return [x for x in src.shapes if x.name == name][0]
def clone(name, x=None, y=None, w=None, h=None, text=None):
    s.shapes._spTree.insert_element_before(copy.deepcopy(shp(name)._element), 'p:extLst'); c = s.shapes[-1]
    if x is not None: c.left, c.top, c.width, c.height = Inches(x), Inches(y), Inches(w), Inches(h)
    if text is not None:
        tf = c.text_frame
        for e in tf.paragraphs[1:]: e._p.getparent().remove(e._p)
        rr = tf.paragraphs[0].runs; rr[0].text = text
        for r in rr[1:]: r._r.getparent().remove(r._r)
    return c
def text(x, y, w, h, lines, align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.MIDDLE):
    tb = s.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h)); tf = tb.text_frame; tf.word_wrap = True; tf.vertical_anchor = anchor
    for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
    for i, line in enumerate(lines):
        para = tf.paragraphs[0] if i == 0 else tf.add_paragraph(); para.alignment = align
        for t, size, col, bold, font in line:
            r = para.add_run(); r.text = t; f = r.font; f.size = Pt(size); f.bold = bold; f.name = font; f.color.rgb = RGBColor.from_string(col)
    return tb
def shape(kind, x, y, w, h, fill=None, line=None, lw=1.0):
    c = s.shapes.add_shape(kind, Inches(x), Inches(y), Inches(w), Inches(h))
    if fill: c.fill.solid(); c.fill.fore_color.rgb = RGBColor.from_string(fill)
    else: c.fill.background()
    if line: c.line.color.rgb = RGBColor.from_string(line); c.line.width = Pt(lw)
    else: c.line.fill.background()
    if kind == MSO_SHAPE.ROUNDED_RECTANGLE: c.adjustments[0] = 0.12 if h < 1 else 0.035
    return c
def icon(name, tag, x, y, d): s.shapes.add_picture(f'{ICONS}/{name}-{tag}.png', Inches(x), Inches(y), Inches(d), Inches(d))

clone('TextBox 135', text='Evidence-led nature insight in hours, governed by EY experts')
clone('TextBox 136', text='EY Climate Change and Sustainability Services  |  Nature Opportunity Agent')
clone('Straight Connector 3')

# ---------------- left: how it works
LX, LW, TOP = 0.58, 5.95, 1.3
clone('left-heading', LX, TOP, 4, 0.3, 'How the agent works')
icon('user-check', 'yellow', LX + 4.42, TOP + 0.03, 0.24); text(LX + 4.72, TOP, 1.25, 0.3, [[('= EY review point', 9, GR, False, L)]])
STEPS = [('files', 'Ingest', 'Client disclosures, national commitments, government strategy', 'EY defines scope and sources', True),
         ('scan-search', 'Extract', 'Every data point sourced to the page', 'EY validates evidence', True),
         ('scale', 'Assess', 'Evidence mapped to nature themes and prioritised', 'EY approves prioritisation', True),
         ('calculator', 'Quantify', 'Formula-driven figures; 27 automated controls', 'Automated; exceptions to EY', False),
         ('file-check', 'Report', 'Client-ready outputs, every claim referenced', 'EY signs off conclusions', True)]
RY, RH, GAP = TOP + 0.45, 0.62, 0.09
ln = s.shapes.add_connector(MSO_CONNECTOR.STRAIGHT, Inches(LX + 0.27), Inches(RY + 0.3), Inches(LX + 0.27), Inches(RY + 4 * (RH + GAP) + 0.3))
ln.line.color.rgb = RGBColor.from_string(Y); ln.line.width = Pt(1.5)
for i, (ic, head, body, hitl, human) in enumerate(STEPS):
    y = RY + i * (RH + GAP)
    shape(MSO_SHAPE.OVAL, LX, y + 0.04, 0.54, 0.54, fill=Y); icon(ic, 'dark', LX + 0.13, y + 0.17, 0.28)
    text(LX + 0.7, y, 2.75, RH, [[(f'{i + 1}  {head}', 11.5, WH, True, B)], [(body, 9, GR, False, L)]])
    chip = shape(MSO_SHAPE.ROUNDED_RECTANGLE, LX + 3.55, y + 0.1, 2.4, 0.42, fill=DARK, line=Y if human else LINE, lw=1.0)
    icon('user-check' if human else 'list-checks', 'yellow' if human else 'grey', LX + 3.65, y + 0.17, 0.28)
    text(LX + 4.0, y + 0.1, 1.9, 0.42, [[(hitl, 9, Y if human else GR, human, B if human else L)]])
# divider
d = s.shapes.add_connector(MSO_CONNECTOR.STRAIGHT, Inches(6.62), Inches(TOP + 0.05), Inches(6.62), Inches(RY + 5 * RH + 4 * GAP))
d.line.color.rgb = RGBColor.from_string(LINE); d.line.width = Pt(1)

# ---------------- right: what it brings you
RX, RW = 6.78, 5.26
clone('left-heading', RX, TOP, 4, 0.3, 'Value to your organisation')
shape(MSO_SHAPE.ROUNDED_RECTANGLE, RX, RY - 0.02, RW, 5 * RH + 4 * GAP + 0.04, fill=CARD, line=Y, lw=1.0)
BEN = [('zap', 'Speed', 'Days of analysis compressed into hours', '2 · 3 · 4'),
       ('link', 'Assurance', 'Every figure traceable to its source', '2 · 4'),
       ('shield-check', 'Rigour', 'Automated controls with expert oversight', '3 · 4 · 5'),
       ('target', 'Insight', 'National nature spend mapped to your portfolio', '1 · 3'),
       ('layers', 'Scalability', 'Repeatable across clients and 11 countries', '1 · 5')]
for i, (ic, head, body, steps) in enumerate(BEN):
    y = RY + i * (RH + GAP)
    shape(MSO_SHAPE.OVAL, RX + 0.2, y + 0.08, 0.46, 0.46, fill=Y); icon(ic, 'dark', RX + 0.31, y + 0.19, 0.24)
    text(RX + 0.82, y, 3.4, RH, [[(head, 12, WH, True, B)], [(body, 9.5, GR, False, L)]])
    text(RX + 4.2, y, 0.9, RH, [[('Steps', 7.5, GR, False, L)], [(steps, 10, Y, True, B)]], PP_ALIGN.CENTER)

# ---------------- bottom: the ask and the guardrail
clone('so-what-panel', 0.58, 5.5, 11.46, 0.55)
pn = clone('so-what', 1.15, 5.52, 10.75, 0.51, 'Share your portfolio data to size opportunities at portfolio and asset level.')
icon('database', 'dark', 0.78, 5.62, 0.3)
text(0.58, 6.25, 11.46, 0.3, [[('The agent recommends; EY decides. Approved Microsoft tools only; public information unless client data is provided.', 8.5, GR, False, L)]], anchor=MSO_ANCHOR.TOP)

lst = p.slides._sldIdLst
for sid in list(lst)[:-1]:
    p.part.drop_rel(sid.get('{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id')); lst.remove(sid)
p.save(OUT); print('saved', OUT)
