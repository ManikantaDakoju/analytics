"""One-slide pitch for the Nature Opportunity Agent, in the EY format of the reviewer's master deck.
Usage: python agent_one_slide.py MASTER.pptx ICON_DIR OUT.pptx   (icons: Lucide, ISC licence)"""
import sys, copy
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.enum.shapes import MSO_SHAPE
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
MASTER, ICONS, OUT = sys.argv[1:4]
Y, DARK, WH, GR, CARD, LINE = 'FFE600', '1A1A24', 'F2F2F2', 'BFBFC7', '2E2E38', '3A3A46'
p = Presentation(MASTER); src = p.slides[1]
s = p.slides.add_slide(src.slide_layout)
for sp in list(s.shapes): sp._element.getparent().remove(sp._element)
def shp(name, nth=0): return [x for x in src.shapes if x.name == name][nth]
def clone(name, x=None, y=None, w=None, h=None, text=None, nth=0):
    s.shapes._spTree.insert_element_before(copy.deepcopy(shp(name, nth)._element), 'p:extLst'); c = s.shapes[-1]
    if x is not None: c.left, c.top, c.width, c.height = Inches(x), Inches(y), Inches(w), Inches(h)
    if text is not None:
        tf = c.text_frame
        for e in tf.paragraphs[1:]: e._p.getparent().remove(e._p)
        rr = tf.paragraphs[0].runs; rr[0].text = text
        for r in rr[1:]: r._r.getparent().remove(r._r)
    return c
def text(x, y, w, h, runs, align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.TOP):
    tb = s.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h)); tf = tb.text_frame; tf.word_wrap = True; tf.vertical_anchor = anchor
    for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
    first = True
    for line in runs:
        para = tf.paragraphs[0] if first else tf.add_paragraph(); first = False; para.alignment = align
        for t, size, col, bold, font in line:
            r = para.add_run(); r.text = t; f = r.font; f.size = Pt(size); f.bold = bold; f.name = font; f.color.rgb = RGBColor.from_string(col)
    return tb
def circle(x, y, d, fill, line=None):
    c = s.shapes.add_shape(MSO_SHAPE.OVAL, Inches(x), Inches(y), Inches(d), Inches(d))
    if fill: c.fill.solid(); c.fill.fore_color.rgb = RGBColor.from_string(fill)
    else: c.fill.background()
    if line: c.line.color.rgb = RGBColor.from_string(line); c.line.width = Pt(1.25)
    else: c.line.fill.background()
    return c
def icon(name, tag, x, y, d): s.shapes.add_picture(f'{ICONS}/{name}-{tag}.png', Inches(x), Inches(y), Inches(d), Inches(d))
B, L = 'EYInterstate', 'EYInterstate Light'

# header (title, line, connector) exactly as the master
clone('TextBox 135', text='Nature Opportunity Agent: evidence-led insight in hours, not weeks')
clone('TextBox 136', text='EY Climate Change and Sustainability Services  |  How the agent works')
clone('Straight Connector 3')

# how it works: four steps
clone('left-heading', 0.58, 1.3, 6, 0.3, 'How it works')
STEPS = [('files', 'Inputs', 'National nature commitments, client reports, government strategies'),
         ('scan-search', 'Extract and trace', 'Every fact captured with its source and page'),
         ('scale', 'Score and match', 'National money matched to the client’s book'),
         ('user-check', 'Review and deliver', 'EY experts review; client-ready slides')]
W, G, X0, Y0, H = 2.55, 0.42, 0.58, 1.72, 2.05
for i, (ic, head, body) in enumerate(STEPS):
    x = X0 + i * (W + G)
    clone('tile-1', x, Y0, W, H)
    circle(x + W / 2 - 0.33, Y0 + 0.2, 0.66, Y); icon(ic, 'dark', x + W / 2 - 0.2, Y0 + 0.33, 0.4)
    text(x + 0.15, Y0 + 1.0, W - 0.3, 0.3, [[(head, 13, WH, True, B)]], PP_ALIGN.CENTER)
    text(x + 0.18, Y0 + 1.35, W - 0.36, 0.62, [[(body, 10, GR, False, L)]], PP_ALIGN.CENTER)
    if i < 3:
        a = s.shapes.add_shape(MSO_SHAPE.CHEVRON, Inches(x + W + 0.12), Inches(Y0 + H / 2 - 0.13), Inches(0.18), Inches(0.26))
        a.fill.solid(); a.fill.fore_color.rgb = RGBColor.from_string(Y); a.line.fill.background()

# why it stands out: five capabilities
clone('left-heading', 0.58, 4.05, 6, 0.3, 'Why it stands out')
CAPS = [('zap', 'Fast', 'Days of review in hours'), ('link', 'Traceable', 'Every number to its source'),
        ('list-checks', 'Checked', '27 automatic quality checks'), ('shield-check', 'Expert-led', 'EY reviews every judgement'),
        ('globe', 'Scalable', '11 countries, any client')]
CW = (11.46 - 4 * 0.12) / 5
for i, (ic, head, body) in enumerate(CAPS):
    x = 0.58 + i * (CW + 0.12); y = 4.45
    circle(x, y, 0.56, None, Y); icon(ic, 'yellow', x + 0.13, y + 0.13, 0.3)
    text(x + 0.68, y + 0.02, CW - 0.7, 0.56, [[(head, 12, WH, True, B)], [(body, 9.5, GR, False, L)]], anchor=MSO_ANCHOR.MIDDLE)

# the ask (yellow panel, as the master's so-what)
clone('so-what-panel', 0.58, 5.45, 11.46, 0.55)
pn = clone('so-what', 0.63, 5.47, 11.37, 0.51, 'Add your portfolio data, and the agent sizes opportunities at portfolio and asset level.')
icon('database', 'dark', 0.78, 5.57, 0.3)
pn.left = Inches(1.15); pn.width = Inches(10.75)
text(0.58, 6.25, 11.46, 0.3, [[('Technology-enabled, expert-led: the agent proposes, EY decides. Approved Microsoft tools; public information unless client data is shared.', 8.5, GR, False, L)]])

# keep only the new slide
lst = p.slides._sldIdLst
for sid in list(lst)[:-1]:
    p.part.drop_rel(sid.get('{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id')); lst.remove(sid)
p.save(OUT); print('saved', OUT)
