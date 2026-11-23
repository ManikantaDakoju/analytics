"""v5.2: the reviewer's deck is the master. Change ONLY Helen's outstanding points (19 November) and plain errors,
in the existing shapes and formatting; add the agent slide (as generated in v5.1, without an abbreviation line).
No speaker notes, source footnotes or abbreviation keys are added.  Usage: python apply_helen_v52.py MASTER.pptx WB_v5.1.xlsx OUT.pptx"""
import sys, copy, openpyxl
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.chart.data import CategoryChartData
from pptx.enum.text import PP_ALIGN
DECK, WB, OUT = sys.argv[1:4]
p = Presentation(DECK); cover, s_uk, s_nx, s_ex = list(p.slides)[:4]
LOG = []
def shp(slide, name, nth=0): return [s for s in slide.shapes if s.name == name][nth]
def run_replace(slide_no, sh, old, new, why):
    for para in sh.text_frame.paragraphs:
        for r in para.runs:
            if old in r.text:
                LOG.append((slide_no, why, r.text, r.text.replace(old, new))); r.text = r.text.replace(old, new); return
    raise SystemExit(f'not found on slide {slide_no}: {old}')
def set_run(slide_no, sh, para, run, new, why):
    r = sh.text_frame.paragraphs[para].runs[run]; LOG.append((slide_no, why, r.text, new)); r.text = new

# ---- slide 1: cover
set_run(1, shp(cover, 'Subtitle 7'), 0, 0, 'For discussion – 10 December 2026', 'Cover date for the client meeting')

# ---- UK context (errors + Helen: PRA covers physical and transition risk)
run_replace(2, shp(s_uk, 'tile-2-text'), 'underwater-use', 'under water-use', 'Typo')
run_replace(2, shp(s_uk, 'callout-text'), '(~ £ 41bn)', '(~£41bn)', 'Spacing')
run_replace(2, shp(s_uk, 'right-sub', 1), 'increases expectation for banks management of physical risk',
            'increases expectations for banks’ management of physical and transition climate risk', 'Helen: SS5/25 covers climate risk in general (physical and transition)')

# ---- UK nature finance and LBG
s = s_nx
set_run(3, shp(s, 'TextBox 135'), 0, 0, 'Three priority themes take 81% of UK nature commitments', 'Helen: title should state the priority themes and their share')
run_replace(3, shp(s, 'Rectangle 2'), 'commitments, the table below maps these priorities and other natural themes', 'commitments; the table below maps these priorities and the other nature themes', 'Wording ("natural" → "nature")')
run_replace(3, shp(s, 'row-1-link'), 'flood risk; Flood-exposed share of mortgage is not disclosed', 'flood risk; flood-exposed mortgage share is not disclosed', 'Grammar (stays on three lines)')
run_replace(3, shp(s, 'so-what'), ' and dependencies across growth sectors', '', 'Helen: growth-sector strip removed, so the so-what no longer refers to it')
fn = shp(s, 'TextBox 19')
for r in fn.text_frame.paragraphs[0].runs: r.font.name = 'EYInterstate Light'
LOG.append((3, 'Footnote font Segoe UI → EYInterstate Light (as the rest of the deck)', '', ''))
# UK priority column: header copied from the score header, ticks styled like the theme rows
src = shp(s, 'col-head-2'); s.shapes._spTree.insert_element_before(copy.deepcopy(src._element), 'p:extLst')
h = s.shapes[-1]; h.name = 'col-head-uk'; h.left, h.top, h.width = Inches(3.02), src.top, Inches(1.12)
rs = h.text_frame.paragraphs[0].runs; rs[0].text = 'UK priority'; rs[1]._r.getparent().remove(rs[1]._r); rs = h.text_frame.paragraphs[0].runs
rs[1].text = ' (≥10% of commitments)'; rs[1].font.size = Pt(8)
TICK = {'Flood, coastal and adaptation': '✓', 'Water': '✓', 'Agriculture and forestry': '✓', 'Urban development': '', 'Pollution and waste': '✓'}
for n in range(1, 6):
    row = shp(s, f'row-{n}-theme'); name = row.text_frame.paragraphs[0].runs[0].text
    tb = s.shapes.add_textbox(Inches(3.02), row.top, Inches(1.12), Inches(0.3)); tb.name = f'row-{n}-uk'
    tf = tb.text_frame
    for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
    r = tf.paragraphs[0].add_run(); r.text = TICK[name]; tf.paragraphs[0].alignment = PP_ALIGN.CENTER
    r.font.name = 'EYInterstate'; r.font.size = Pt(14); r.font.bold = True; r.font.color.rgb = RGBColor(0xFF, 0xE6, 0x00)
LOG.append((3, 'Helen: tick where the theme is a UK priority (new column; ✓ = theme takes 10% or more of identified UK commitments: flood 38%, water 31%, pollution 18%, agriculture 12%; not urban 1%)', '', ''))
# growth strip -> 'Beyond the top three' (same cards, same three-line format)
for name in ('Group 11', 'Group 10'):
    g = shp(s, name); g._element.getparent().remove(g._element)
run_replace(3, shp(s, 'TextBox 17'), 'UK Growth Sectors', 'Beyond the top three', 'Helen: remove the growth sectors and ENCORE scores')
CARDS = [('Group 13', 2.40, 4.70, 'Urban development', 'Construction £5.9bn (0.9%), a nature-priority sector for LBG', 'Mandatory biodiversity net gain: finance for housebuilders'),
         ('Group 12', 7.30, 4.86, 'Pollution and waste', '18% of UK commitments (£10.8bn); weak link in LBG’s book', 'Room to grow waste and circular-economy finance')]
for name, x, w, head, l2, l3 in CARDS:
    g = shp(s, name); g.left, g.width = Inches(x), Inches(w)
    t = [c for c in g.shapes if c.name.endswith('-text')][0].text_frame.paragraphs
    for para, txt in zip(t, (head, l2, l3)):
        para.runs[0].text = txt
        for r in para.runs[1:]: r._r.getparent().remove(r._r)
LOG.append((3, 'Helen: anything for urban development and pollution and waste? Two cards replace the four growth-sector cards', '', ''))

# ---- LBG portfolio
s = s_ex
set_run(4, shp(s, 'TextBox 135'), 0, 0, 'Nature-priority sectors make up 9.3% (£60.0bn) of LBG’s book', 'Helen: title should focus on the yellow (nature-priority) sectors, not mortgages')
run_replace(4, shp(s, 'exp-sub'), '£ 645.1bn', '£645.1bn', 'Spacing')
run_replace(4, shp(s, 'exp-legend'), 'Yellow marker = sector identified by LBG as a nature priority', 'Yellow = share of each sector identified by LBG as a nature priority', 'Error: yellow is the nature-priority share, not the whole sector')
run_replace(4, shp(s, 'exp-text', 0), 'Mortgages are outside this sector classification', 'LBG classes mortgages as 0% nature priority', 'Error: mortgages are inside LBG’s classification, at 0%')
set_run(4, shp(s, 'cell-text-3-3'), 0, 0, 'Connect Soil Association Exchange farm scorecards to LBG’s existing sustainability-linked loans, and report lending volumes.', 'Helen: LBG already offers sustainability-linked loans with KPIs (financing framework, page 24), so the opportunity is to connect and report')
dc = openpyxl.load_workbook(WB, data_only=True)['Deck_Chart']
lab = {dc.cell(r, 1).value.split('  ')[0]: dc.cell(r, 1).value for r in range(30, 42) if dc.cell(r, 1).value}
ch = shp(s, 'chart-D-book').chart; pl = ch.plots[0]
cats = list(pl.categories); ser = [(se.name, list(se.values)) for se in pl.series]
new = [lab[c.split('  ')[0]] for c in cats]
cd = CategoryChartData(); cd.categories = new
for nm, v in ser: cd.add_series(nm, v)
ch.replace_data(cd)
LOG.append((4, 'Your request: chart labels show the amount as well as the share (e.g. "' + new[0] + '")', '', ''))

# ---- agent slide (as generated in v5.1, no abbreviation line), placed after the cover
ns = p.slides.add_slide(s_uk.slide_layout)
for sp in list(ns.shapes): sp._element.getparent().remove(sp._element)
def clone(src, x, y, w, h, text=None):
    ns.shapes._spTree.insert_element_before(copy.deepcopy(src._element), 'p:extLst'); sh = ns.shapes[-1]
    sh.left, sh.top, sh.width, sh.height = Inches(x), Inches(y), Inches(w), Inches(h)
    if text is not None:
        tf = sh.text_frame
        for extra in tf.paragraphs[1:]: extra._p.getparent().remove(extra._p)
        rr = tf.paragraphs[0].runs; rr[0].text = text
        for r in rr[1:]: r._r.getparent().remove(r._r)
    return sh
for nm in ('TextBox 135', 'TextBox 136', 'Straight Connector 3'):
    ns.shapes._spTree.insert_element_before(copy.deepcopy(shp(s_uk, nm)._element), 'p:extLst')
def first(sh, t):
    tf = sh.text_frame
    for extra in tf.paragraphs[1:]: extra._p.getparent().remove(extra._p)
    rr = tf.paragraphs[0].runs; rr[0].text = t
    for r in rr[1:]: r._r.getparent().remove(r._r)
first(shp(ns, 'TextBox 135'), 'An agent that matches national nature priorities to a client’s book')
first(shp(ns, 'TextBox 136'), 'Lloyds Banking Group  |  What the nature opportunity agent does')
tile, stat, txt = shp(s_uk, 'tile-1'), shp(s_uk, 'tile-1-stat'), shp(s_uk, 'tile-1-text')
STEPS = [('1  Inputs', 'EY research on national nature commitments (11 countries), the client’s public documents and official government strategies'),
         ('2  Extract and trace', 'A Microsoft Copilot agent pulls facts with exact quotes and page references. Every number is calculated in a controlled Excel workbook, not typed'),
         ('3  Score and match', 'Five nature themes are scored for the client, and national money is matched to the client’s lending by sector'),
         ('4  Review and output', 'The EY team checks every fact, score and view; 27 automatic checks run before the draft slides are produced')]
for i, (head, body) in enumerate(STEPS):
    x = 0.58 + i * 2.92
    clone(tile, x, 1.45, 2.72, 2.45); hs = clone(stat, x + 0.12, 1.6, 2.48, 0.45, head); clone(txt, x + 0.15, 2.2, 2.42, 1.55, body)
    for r in hs.text_frame.paragraphs[0].runs: r.font.size = Pt(15)
clone(shp(s_uk, 'left-heading'), 0.58, 4.15, 11.46, 0.3, 'What it shows')
tb = ns.shapes.add_textbox(Inches(0.58), Inches(4.5), Inches(11.46), Inches(0.75)); tf = tb.text_frame; tf.word_wrap = True
for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
r = tf.paragraphs[0].add_run(); r.text = ('Using public information only, the following pages show where UK nature money is going, how it connects to LBG’s lending, and the areas worth exploring. '
    'With LBG’s own data (lending by sector, region and product, and property locations), the same approach can size opportunities at portfolio and asset level.')
r.font.name = 'EYInterstate Light'; r.font.size = Pt(11); r.font.color.rgb = RGBColor(0xF2, 0xF2, 0xF2)
clone(shp(s_uk, 'so-what-panel'), 0.58, 5.55, 11.46, 0.52)
clone(shp(s_uk, 'so-what'), 0.63, 5.57, 11.37, 0.48, 'A conversation starter: what the national data says about your book, and where we could help you go further.')
tb = ns.shapes.add_textbox(Inches(0.58), Inches(6.5), Inches(11.46), Inches(0.3)); tf = tb.text_frame
for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
r = tf.paragraphs[0].add_run(); r.text = 'Draft output of the agent, reviewed by the EY team. Public information only; national figures are indicative.'
r.font.name = 'EYInterstate Light'; r.font.size = Pt(8); r.font.italic = True; r.font.color.rgb = RGBColor(0xBF, 0xBF, 0xC7)
lst = p.slides._sldIdLst; ids = list(lst); lst.remove(ids[-1]); lst.insert(1, ids[-1])
LOG.append(('new 2', 'Helen: a slide on what the agent does, so LBG can see how the analysis was done', '', ''))
p.save(OUT)
import json; json.dump(LOG, open(OUT + '.changes.json', 'w'), ensure_ascii=False, indent=1); print('saved', OUT, len(LOG), 'changes')
