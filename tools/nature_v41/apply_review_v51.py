"""v5.1: apply the reviewed content (Helen, 19 November) to the user's styled deck, keeping its formatting.
  python apply_review_v51.py USER_DECK.pptx WORKBOOK_v5.1.xlsx OUT.pptx   -> filled LBG deck (values from Deck_Tokens)
  python apply_review_v51.py USER_DECK.pptx --skeleton OUT.pptx           -> the same design with {{placeholders}}: the v5.1 skeleton
Adds the 'what the agent does' slide (built from the deck's own tile styles), the UK priority tick column, the
'beyond the top three' strip, footnotes, abbreviation keys and speaker notes."""
import sys, copy, re, openpyxl
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.chart.data import CategoryChartData
from pptx.enum.text import PP_ALIGN, MSO_AUTO_SIZE
DECK, WB, OUT = sys.argv[1:4]
SKEL = WB == '--skeleton'
class Tok(dict):
    def __missing__(self, k): return '{{' + k + '}}'
if SKEL:
    T = Tok(); wb = None
else:
    wb = openpyxl.load_workbook(WB, data_only=True)
    T = {r[0].strip('{}'): ('' if r[1] is None else str(r[1])) for r in wb['Deck_Tokens'].iter_rows(min_row=5, values_only=True) if r[0]}
p = Presentation(DECK)
cover, s_uk, s_nx, s_ex = list(p.slides)[:4]

def shp(slide, name, nth=0):
    return [s for s in slide.shapes if s.name == name][nth]
def set_text(sh, text):
    """Replace the text, keep the formatting of the first run."""
    tf = sh.text_frame; p0 = tf.paragraphs[0]
    for extra in tf.paragraphs[1:]: extra._p.getparent().remove(extra._p)
    if not p0.runs: p0.add_run()
    p0.runs[0].text = text
    for r in p0.runs[1:]: r._r.getparent().remove(r._r)
def add_box(slide, x, y, w, h, text, size, color='BFBFC7', bold=False, italic=False, font='EYInterstate Light', align=None, name=None):
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h)); tf = tb.text_frame; tf.word_wrap = True
    for m in ('margin_left', 'margin_right', 'margin_top', 'margin_bottom'): setattr(tf, m, 0)
    r = tf.paragraphs[0].add_run(); r.text = text; f = r.font; f.size = Pt(size); f.name = font; f.bold = bold; f.italic = italic; f.color.rgb = RGBColor.from_string(color)
    if align: tf.paragraphs[0].alignment = align
    if name: tb.name = name
    return tb
def notes(slide, text): slide.notes_slide.notes_text_frame.text = text
def line(sec): return T['client_name'] + '  |  ' + T[sec]

# ---------- cover
set_text(shp(cover, 'Title 8'), T['cover_title']); set_text(shp(cover, 'Subtitle 7'), T['cover_date'])

# ---------- UK context
s = s_uk
set_text(shp(s, 'TextBox 135'), T['s1_title']); set_text(shp(s, 'TextBox 136'), line('s1_section'))
set_text(shp(s, 'TextBox 2'), T['s1_ctx_what']); set_text(shp(s, 'TextBox 4'), T['s1_ctx_measured']); set_text(shp(s, 'TextBox 6'), T['s1_ctx_why'])
set_text(shp(s, 'left-heading'), T['s1_left_head']); set_text(shp(s, 'left-sub'), T['s1_left_sub'])
c = shp(s, 'callout-text').text_frame.paragraphs[0].runs; c[0].text = T['s1_callout_stat']; c[1].text = '  ' + T['s1_callout_text']
for r in c[2:]: r._r.getparent().remove(r._r)
set_text(shp(s, 'right-heading'), T['s1_heading_r']); set_text(shp(s, 'right-sub', 0), T['s1_sub_r'])
for n in (1, 2, 3, 4):
    set_text(shp(s, f'tile-{n}-stat'), T[f's1_tile{n}_stat']); set_text(shp(s, f'tile-{n}-text'), T[f's1_tile{n}_text'])
set_text(shp(s, 'rules'), T['s1_rules_lead']); set_text(shp(s, 'right-sub', 1), T['s1_rules'])
set_text(shp(s, 'so-what'), T['s1_sowhat']); set_text(shp(s, 'footnote'), T['s1_footnote'])
add_box(s, 0.58, 6.88, 11.46, 0.26, T['s1_key'], 6.5, name='abbreviation-key')
notes(s, T['s1_notes'])

# ---------- national finance and the client
s = s_nx
set_text(shp(s, 'TextBox 135'), T['s2_title']); set_text(shp(s, 'TextBox 136'), line('s2_section'))
set_text(shp(s, 'Rectangle 2'), T['s2_lead'])
tag_src = copy.deepcopy(shp(s, 'row-1-theme').text_frame.paragraphs[1]._p)
for n in range(1, 6):
    tf = shp(s, f'row-{n}-theme').text_frame
    if len(tf.paragraphs) == 1 and T[f'r{n}_align']: tf._txBody.append(copy.deepcopy(tag_src))
    th = tf.paragraphs; th[0].runs[0].text = T[f'r{n}_theme']
    if len(th) > 1:
        if T[f'r{n}_align']: th[1].runs[0].text = T[f'r{n}_align']
        else: th[1]._p.getparent().remove(th[1]._p)
    set_text(shp(s, f'row-{n}-money'), T[f'r{n}_money']); set_text(shp(s, f'row-{n}-link'), T[f'r{n}_link'])
hd = shp(s, 'col-head-1'); s.shapes._spTree.insert_element_before(copy.deepcopy(hd._element), 'p:extLst')
h2 = s.shapes[-1]; h2.name = 'col-head-uk'; h2.left, h2.top, h2.width, h2.height = Inches(3.05), hd.top, Inches(1.05), hd.height
set_text(h2, T['country_label'] + ' priority²'); h2.text_frame.paragraphs[0].alignment = PP_ALIGN.CENTER
for n in range(1, 6):
    row = shp(s, f'row-{n}-theme')
    add_box(s, 3.05, row.top / 914400, 1.05, 0.3, T[f'r{n}_uk'], 14, color='FFE600', bold=True, font='EYInterstate', align=PP_ALIGN.CENTER, name=f'row-{n}-uk')
for name in ('Group 11', 'Group 10'):
    g = shp(s, name); g._element.getparent().remove(g._element)
set_text(shp(s, 'TextBox 17'), 'Beyond the top three')
for name, n, x, w in (('Group 13', 1, 2.40, 4.70), ('Group 12', 2, 7.30, 4.86)):
    g = shp(s, name); g.left, g.width = Inches(x), Inches(w)
    t = [c for c in g.shapes if c.name.endswith('-text')][0].text_frame.paragraphs
    t[0].runs[0].text = T[f's2_other{n}_head']
    for r in t[0].runs[1:]: r._r.getparent().remove(r._r)
    t[1].runs[0].text = T[f's2_other{n}_text']
    for r in t[1].runs[1:]: r._r.getparent().remove(r._r)
    for extra in t[2:]: extra._p.getparent().remove(extra._p)
set_text(shp(s, 'so-what'), T['s2_sowhat'])
fn = shp(s, 'TextBox 19'); fn.left = Inches(0.58); fn.width = Inches(11.46); fn.height = Inches(0.25); fn.top = Inches(6.73)
set_text(fn, T['s2_footnote']); fn.text_frame.auto_size = MSO_AUTO_SIZE.NONE; fn.text_frame.word_wrap = True
fn.text_frame.paragraphs[0].alignment = PP_ALIGN.LEFT
for r in fn.text_frame.paragraphs[0].runs:
    r._r.get_or_add_rPr().attrib.pop('baseline', None)
    r.font.name = 'EYInterstate Light'; r.font.size = Pt(7); r.font.color.rgb = RGBColor.from_string('BFBFC7')
add_box(s, 1.2, 7.08, 10.84, 0.16, T['s2_key'], 6.5, name='abbreviation-key')
notes(s, T['s2_notes'])

# ---------- client portfolio
s = s_ex
set_text(shp(s, 'TextBox 135'), T['s3_title']); set_text(shp(s, 'TextBox 136'), line('s3_section'))
set_text(shp(s, 'exp-sub'), T['s3_exp_sub'])
set_text(shp(s, 'exp-legend'), 'Bar length = share of ' + T['client_short'] + '’s total exposure | Yellow = part of each sector ' + T['client_short'] + ' classes as a nature priority')
set_text(shp(s, 'exp-stat'), T['s3_exp_stat']); set_text(shp(s, 'exp-text', 1), T['s3_exp_text'])
note = shp(s, 'exp-text', 0).text_frame.paragraphs[0].runs
note[0].text = 'Note'; note[1].text = ': ' + T['s3_exp_note']
for r in note[2:]: r._r.getparent().remove(r._r)
for n in (1, 2, 3):
    set_text(shp(s, f'col-head-text-{n}'), T[f'h{n}'])
    for key, row in (('today', 1), ('gap', 2), ('opp', 3)): set_text(shp(s, f'cell-text-{row}-{n}'), T[f's3_col{n}_{key}'])
set_text(shp(s, 'so-what'), T['s3_sowhat'])
add_box(s, 0.58, 6.79, 11.46, 0.28, T['s3_footnote'], 7.5, italic=True, name='footnote')
add_box(s, 1.20, 7.08, 10.84, 0.16, T['s3_key'], 6.5, name='abbreviation-key')
notes(s, T['s3_notes'])

# ---------- charts: workbook data, or neutral placeholder data in the skeleton
def chart(slide, name, rows, s1, s2):
    cd = CategoryChartData(); cd.categories = [r[0] for r in rows]
    cd.add_series(s1, [float(r[1] or 0) for r in rows]); cd.add_series(s2, [float(r[2] or 0) for r in rows]); shp(slide, name).chart.replace_data(cd)
if SKEL:
    chart(s_uk, 'chart-A-money-by-theme', [[f'Theme {i} (0%)', 1.0 if i <= 2 else 0, 0 if i <= 2 else 1.0] for i in range(1, 6)], 'Top 2', 'Other')
    chart(s_nx, 'chart-B-score', [[f'Theme {i}', 4.0 if i <= 3 else 0, 0 if i <= 3 else 3.0] for i in range(1, 6)], 'High', 'Medium')
    chart(s_ex, 'chart-D-book', [[f'Sector {i}  0.0bn | 0.0%', 0.0, round(0.5 / i, 3)] for i in range(1, 13)], 'Nature-priority part', 'Rest of sector')
else:
    dc = wb['Deck_Chart']
    for sl, name, h, a, b in ((s_uk, 'chart-A-money-by-theme', 5, 6, 10), (s_nx, 'chart-B-score', 13, 14, 18), (s_ex, 'chart-D-book', 29, 30, 41)):
        rows = [[dc.cell(r, c).value for c in (1, 2, 3)] for r in range(a, b + 1) if dc.cell(r, 1).value]
        chart(sl, name, rows, dc.cell(h, 2).value, dc.cell(h, 3).value)

# ---------- new slide 2: what the agent does (built from the deck's own tile styles)
new = p.slides.add_slide(s_uk.slide_layout)
for sp in list(new.shapes): sp._element.getparent().remove(sp._element)
def clone(src, x, y, w, h, text=None):
    new.shapes._spTree.insert_element_before(copy.deepcopy(src._element), 'p:extLst'); sh = new.shapes[-1]
    sh.left, sh.top, sh.width, sh.height = Inches(x), Inches(y), Inches(w), Inches(h)
    if text is not None: set_text(sh, text)
    return sh
for nm in ('TextBox 135', 'TextBox 136', 'Straight Connector 3'):
    new.shapes._spTree.insert_element_before(copy.deepcopy(shp(s_uk, nm)._element), 'p:extLst')
set_text(shp(new, 'TextBox 135'), 'An agent that matches national nature priorities to a client’s book')
set_text(shp(new, 'TextBox 136'), T['client_name'] + '  |  What the nature opportunity agent does')
tile, stat, txt = shp(s_uk, 'tile-1'), shp(s_uk, 'tile-1-stat'), shp(s_uk, 'tile-1-text')
STEPS = [('1  Inputs', 'EY research on national nature commitments (11 countries), the client’s public documents and official government strategies'),
         ('2  Extract and trace', 'A Microsoft Copilot agent pulls facts with exact quotes and page references. Every number is calculated in a controlled Excel workbook, not typed'),
         ('3  Score and match', 'Five nature themes are scored for the client, and national money is matched to the client’s lending by sector'),
         ('4  Review and output', 'The EY team checks every fact, score and view; 27 automatic checks run before the draft slides are produced')]
W, G, X0, Y0, H = 2.72, 0.2, 0.58, 1.45, 2.45
for i, (head, body) in enumerate(STEPS):
    x = X0 + i * (W + G)
    clone(tile, x, Y0, W, H); hs = clone(stat, x + 0.12, Y0 + 0.15, W - 0.24, 0.45, head); clone(txt, x + 0.15, Y0 + 0.75, W - 0.3, H - 0.9, body)
    for r in hs.text_frame.paragraphs[0].runs: r.font.size = Pt(15)
clone(shp(s_uk, 'left-heading'), 0.58, 4.15, 11.46, 0.3, 'What it shows')
add_box(new, 0.58, 4.5, 11.46, 0.75, 'Using public information only, the following pages show where ' + T['country_label'] + ' nature money is going, how it connects to ' + T['client_short'] +
        '’s lending, and the areas worth exploring. With ' + T['client_short'] + '’s own data (lending by sector, region and product, and property locations), the same approach can size opportunities at portfolio and asset level.', 11, color='F2F2F2')
clone(shp(s_uk, 'so-what-panel'), 0.58, 5.55, 11.46, 0.52)
clone(shp(s_uk, 'so-what'), 0.63, 5.57, 11.37, 0.48, 'A conversation starter: what the national data says about your book, and where we could help you go further.')
add_box(new, 0.58, 6.5, 11.46, 0.3, 'Draft output of the agent, reviewed by the EY team. Public information only; national figures are indicative.', 8, italic=True)
add_box(new, 0.58, 6.88, 11.46, 0.16, 'Abbreviations: EY = Ernst & Young; ' + T['client_short'] + ' = ' + T['client_name'] + '; ' + T['country_label'] + ' = ' +
        ('{{country_name}}' if SKEL else ('United Kingdom' if T['country_label'] == 'UK' else T['country_label'])), 6.5, name='abbreviation-key')
notes(new, 'Open with this page: what the agent is and what goes into it. The next three pages are evidence of what it produces from public information. '
      'Close by asking what the client could share (lending data by sector, region and product) to take it further.')
lst = p.slides._sldIdLst; ids = list(lst); lst.remove(ids[-1]); lst.insert(1, ids[-1])

# ---------- skeleton: make the remaining fixed client / country words generic
if SKEL:
    for sl in p.slides:
        for sh in sl.shapes:
            frames = list(sh.shapes) if sh.shape_type == 6 else [sh]
            for f in frames:
                if not f.has_text_frame: continue
                for para in f.text_frame.paragraphs:
                    for r in para.runs:
                        t = r.text.replace('Lloyds Banking Group', '{{client_name}}')
                        t = re.sub(r'(?<![{A-Za-z_])LBG(?![A-Za-z_}])', '{{client_short}}', t)
                        r.text = re.sub(r'(?<![{A-Za-z_])UK(?![A-Za-z_}])', '{{country_label}}', t)
p.save(OUT); print('saved', OUT)
