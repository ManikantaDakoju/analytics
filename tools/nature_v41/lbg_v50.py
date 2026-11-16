"""v5.0 LBG example: full forms on the slides, abbreviation key, page 4 portfolio-exposure panel, draft cover. All traced.
Usage: python lbg_v50.py LBG_v4.5.xlsx DATA_SHEET.xlsx OUT.xlsx"""
import openpyxl, sys, re
from openpyxl.styles import Font
from patch_v50 import patch50
SRC, DS, OUT = sys.argv[1:4]
wb = openpyxl.load_workbook(SRC); patch50(wb)
BL = Font(name='Arial', size=10, color='0000FF'); TODAY = '16/11/2026'
def setrow(ws, r, vals, c0=1):
    for j, v in enumerate(vals, c0):
        if v is not None: x = ws.cell(r, j); x.value = v; x.font = BL
def nextrow(ws, col=1, start=5):
    r = start
    while ws.cell(r, col).value not in (None, ''): r += 1
    return r
def find(ws, val, col=1, start=5, end=500):
    return next(r for r in range(start, end) if ws.cell(r, col).value == val)

wb['Inputs']['B10'] = 'Internal review (presented)'; wb['Inputs']['B11'] = TODAY

# ---------- 1. Evidence: every sector total as the full disclosed row (from the data sheet, by script) ----------
ds = openpyxl.load_workbook(DS, data_only=True)['18 - Environmental risk']
def dsrow(r):
    name = re.sub(r'\s+\d+$', '', ds.cell(r, 2).value or ds.cell(r - 1, 2).value).replace('  ', ' ').strip()
    F, G, Hh, J, L = (ds.cell(r, c).value for c in (6, 7, 8, 10, 12))
    return name, F, G, Hh, J, L, f'{name} | Total | {F:,} | {G:,} | {Hh:,} | {J:.1%} | {L:.1%}'
# data-sheet 'Total' row for each sector (the name sits on the first row of each block)
blocks = {}
cur = None
for r in range(7, 60):
    if ds.cell(r, 2).value: cur = re.sub(r'\s+\d+$', '', ds.cell(r, 2).value).replace('  ', ' ').strip()
    if ds.cell(r, 4).value == 'Total' and cur: blocks[cur] = r
assert len(blocks) == 12, blocks
COLS = ' Columns: lending | off-balance sheet | total exposure (£m) | increased climate risk % of Group | nature priority sector % of Group, 31 December 2025.'
EVMAP = {'EV-075': 'Agriculture, forestry and fishing', 'EV-039': 'Construction', 'EV-038': 'Energy and water supply',
         'EV-099': 'Financial, business and other services', 'EV-045': 'Manufacturing', 'EV-100': 'Postal and telecommunications',
         'EV-068': 'Property companies', 'EV-040': 'Personal: Mortgages',
         'EV-116': 'Mining and quarrying', 'EV-117': 'Transport, distribution and hotels', 'EV-118': 'Personal: Lease financing', 'EV-119': 'Personal: Other'}
E = wb['Evidence_Log']; M = wb['NBSAP_Mapping']; CE = wb['Client_Exposure']
tot = sum(ds.cell(blocks[n], 8).value for n in blocks); assert tot == ds.cell(60, 8).value == 645058
vals = {}
for ev, name in EVMAP.items():
    r = blocks[name]; F, G, Hh, J, L = (ds.cell(r, c).value for c in (6, 7, 8, 10, 12))
    q = f'{name} | Total | {F:,} | {G:,} | {Hh:,} | {J:.1%} | {L:.1%}'; vals[ev] = (Hh, J, L)
    try:
        x = find(E, ev)
        E.cell(x, 5).value = q; E.cell(x, 3).value = 'Tab 18 – Environmental risk'
        E.cell(x, 8).value = (E.cell(x, 8).value or '').split(' Columns:')[0] + COLS
        E.cell(x, 9).value = f'Figure checked vs data sheet by script ({TODAY})'
    except StopIteration:
        x = nextrow(E)
        setrow(E, x, [ev, 'SRC-04', 'Tab 18 – Environmental risk', '', q, 'Exposure', 'Value chain / portfolio',
                      f'{name} total exposure £{Hh:,}m.' + COLS, f'Figure checked vs data sheet by script ({TODAY})'])
        m = nextrow(M); tpl = find(M, 'EV-100')
        for c in range(1, 10):
            v = M.cell(tpl, c).value
            M.cell(m, c).value = v.replace(f'{tpl}', f'{m}').replace(f'{tpl-1}', f'{m-1}') if isinstance(v, str) and v.startswith('=') else v
        M.cell(m, 1).value = ev; M.cell(m, 8).value = 'Exposure for the page 4 book-by-sector chart (EY view: no direct NBSAP link)'
        c = nextrow(CE)
        setrow(CE, c, [ev, 'SRC-04', name, 'Total', Hh, 'GBP', 'Yes' if L > 0 else 'No', 'Sector total', '—', 'Tab 18'])
# footnote 11, second half (why mortgages are not a nature priority sector)
fn11 = ds['B79'].value
q11 = 'from a nature perspective the direct influence of homeowners on the state of nature is considered to be low'
assert q11 in fn11 and len(q11.split()) <= 25
x = nextrow(E)
setrow(E, x, ['EV-120', 'SRC-04', 'Tab 18 – footnote 11', '', q11, 'Disclosure', 'Value chain / portfolio',
              'LBG does not class mortgages as a nature priority sector: homeowners’ direct influence on nature is low.', f'Text checked vs data sheet by script ({TODAY})'])
m = nextrow(M); tpl = find(M, 'EV-100')
for c in range(1, 10):
    v = M.cell(tpl, c).value
    M.cell(m, c).value = v.replace(f'{tpl}', f'{m}').replace(f'{tpl-1}', f'{m-1}') if isinstance(v, str) and v.startswith('=') else v
M.cell(m, 1).value = 'EV-120'; M.cell(m, 3).value = 'Client position'; M.cell(m, 4).value = 'XC'; M.cell(m, 7).value = 'Direct'
M.cell(m, 8).value = 'How LBG classifies mortgages for nature (page 4 exposure panel)'
# disclosed shares into Client_Exposure K:L for every sector total
for r in range(5, 201):
    ev = CE.cell(r, 1).value
    if ev in vals:
        _, J, L = vals[ev]; CE.cell(r, 11).value = L; CE.cell(r, 12).value = J; CE.cell(r, 11).font = BL; CE.cell(r, 12).font = BL

# fix legacy rows (v4) where Mapping strength (G) and Rationale (H) were swapped
for r in range(5, 305):
    g_, h_ = M.cell(r, 7).value, M.cell(r, 8).value
    if M.cell(r, 1).value and g_ not in ('Direct', 'Indirect') and h_ in ('Direct', 'Indirect'):
        M.cell(r, 7).value, M.cell(r, 8).value = h_, g_
# ---------- 2. Calculations ----------
C = wb['Calculations']; c = nextrow(C, 1, 30)
setrow(C, c, ['CALC-59', 'Nature priority sectors as % of Group exposure', '=VLOOKUP("EV-034",Client_Exposure!$A:$E,5,FALSE)/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE)', '%', '59,991 ÷ 645,058', 'EV-034, EV-067', 'SRC-04'])
C.cell(c, 3).number_format = '0.000'

# ---------- 3. Glossary: key text ----------
G = wb['Glossary']
KT = {'ABB-LBG': 'LBG = Lloyds Banking Group', 'ABB-UK': 'UK = United Kingdom', 'ABB-PRA': 'PRA = Prudential Regulation Authority',
      'ABB-SS525': 'SS5/25 = Supervisory Statement 5/25', 'ABB-AI': 'AI = artificial intelligence', 'ABB-HARP': 'HARP = Haweswater Aqueduct Resilience Programme',
      'ABB-BNG': 'BNG = biodiversity net gain', 'ABB-KPI': 'KPI = key performance indicator'}
for r in range(5, 155):
    i = G.cell(r, 1).value
    if i in KT: G.cell(r, 7).value = KT[i]; G.cell(r, 7).font = BL
    if i == 'DEF-NOTVISIBLE': G.cell(r, 2).value = 'Questions to explore (not yet visible)'
    if i == 'ABB-ENC2': G.cell(r, 3).value = 'Definition'   # product name; ENCORE itself is keyed by ABB-ENCORE
    if i == 'DEF-NOTATION':
        G.cell(r, 2).value = '£bn, £m'; G.cell(r, 4).value = '£bn = billion pounds sterling; £m = million pounds sterling (the abbreviations "c.", "e.g." and "ha" are no longer used on the slides)'

# ---------- 4. Slide text (paste area J:L) ----------
s = wb['Slide_Content']; rows = {s.cell(x, 10).value: x for x in range(5, 121) if s.cell(x, 10).value}
lim = {s.cell(x, 1).value: s.cell(x, 5).value for x in range(5, 200) if s.cell(x, 1).value}
def put(k, v=None, cl=None):
    if k not in rows: rows[k] = nextrow(s, 10)
    x = rows[k]; s.cell(x, 10).value = k; s.cell(x, 10).font = BL
    if v is not None:
        assert len(v) <= (lim.get(k) or 9999), (k, len(v), lim.get(k)); s.cell(x, 11).value = v; s.cell(x, 11).font = BL
    if cl is not None: s.cell(x, 12).value = cl; s.cell(x, 12).font = BL
T = s.cell(rows['s1_talk'], 11).value.replace('Of c.£60bn', 'Of about £60bn'); put('s1_talk', T)
put('cover_date', 'Draft for internal review – 19 November 2026')
put('s1_callout_text', 'goes to water and flood together (about £41bn)')
put('s1_tile1_text', 'UK 10-year infrastructure pipeline, including £7.9bn for flood management (2026–36)')
put('s1_rules', '10% biodiversity net gain mandatory since February 2024; PRA SS5/25 raises physical-risk expectations for banks.', 'C-028, C-030, C-031, C-032')
put('s1_footnote', 'Sources: EY NBSAP Nature Sizing workbook (indicative); Modern Industrial Strategy (2025) page 115; Environment Agency, National Framework for Water Resources 2025, pages 3, 6; Global Canopy, ENCORE Insights Country Dashboard (2026); LBG Built to Last (2026) pages 7, 10, 11; LBG Working with nature (2024) pages 7, 13.')
put('s2_section', 'UK nature finance and LBG')
put('s2_link_DM', 'Mortgages £365.6bn (56.7%) and property £26.3bn (4.1%). Flood exposure of the book not disclosed.')
put('s2_link_W', 'Energy and water supply £12.1bn (1.9%); £120m to the Haweswater Aqueduct Resilience Programme.', 'C-047, C-048, C-049')
put('s2_link_UD', 'Construction £5.9bn (0.9%), including £2.3bn housebuilders.')
put('s3_title', 'Mortgages are 56.7% of LBG’s book; the gaps are targets and data', 'C-001, C-147, C-070')
put('s3_section', 'LBG portfolio exposure and position')
put('s3_exp_stat', '9.3%', 'C-150')
put('s3_exp_sub', 'Share of total exposure (£645.1bn) by sector, 31 December 2025', 'C-151, C-152, C-153')
put('s3_exp_text', 'of the book (£60.0bn) is in sectors LBG classes as nature priorities. Mortgages count as climate risk, not nature (homeowners’ influence is low).', 'C-154, C-155, C-001, C-156, C-157')
put('s3_col1_today', 'Underwrote £238.5m for Cleve Hill (coastal flood defences) with NatWest. Flood doors and barriers eligible for finance. Built to Last report (2026).')
put('s3_col1_gap', 'Flood exposure of the £365.6bn mortgage book (its climate flag reflects home energy use). Volume of resilience lending.')
put('s3_col1_opp', 'Resilience and remediation lending for homes and property, such as flood retrofit and subsidence repair.')
put('s3_col2_today', '£120m of debt to the Haweswater Aqueduct Resilience Programme (£3bn; about 2.5m people). Wastewater treatment, reservoirs and wetlands eligible.', 'C-049, C-078, C-079, C-080, C-081, C-082')
put('s3_col2_opp', 'Finance drought resilience, storm overflows and water efficiency for growth sectors such as data centres.')
put('s3_col3_today', 'Ten-year Soil Association Exchange partnership: 685 farms, 238,494 hectares. Regenerative farming and grant-backed projects eligible.')
put('s3_col3_opp', 'Nature-linked farm lending, with soil and water outcomes as key performance indicators for loans.', 'C-090')
put('s3_footnote', 'Sources: LBG Working with nature (2024) page 4; Built to Last (2026) pages 14–17; Sustainability Data Sheet 2025, tabs 18 and 20; Sustainable Financing Framework 2025 pages 11–15. "Questions to explore" = not found in these four documents; to be checked against the Annual and Sustainability Reports 2025. Scores from page 3.')
put('s3_talk', 'Start with LBG’s own book. Mortgages are 56.7% of total exposure. LBG flags them for climate risk because of home energy use, but not as a nature-priority sector, because homeowners’ direct influence on nature is low. Only 9.3% (£60.0bn) sits in sectors LBG itself classes as nature priorities. So the link to UK nature money on page 3 runs through physical flood risk in the mortgage book, which LBG does not yet disclose. Then credit LBG: the Haweswater Aqueduct Resilience Programme, Cleve Hill, the Soil Association partnership and a financing framework that already covers water, flood resilience and regenerative farming. Then the questions to explore (not found in public documents, not criticisms): flood exposure of the book, lending volumes and nature targets. Then the opportunity row. To make this LBG-specific, the next steps are LBG’s own lending data by sector, region and product, then collateral locations against flood and drought maps. Keep it on opportunity, not risk.')

# ---------- 5. Claims ----------
CR = wb['Claims_Register']
def claim(cid, **kw):
    r = find(CR, cid, end=450); cols = dict(claim=4, type=5, primary=6, other=7, deriv=8)
    for k, v in kw.items(): CR.cell(r, cols[k]).value = v; CR.cell(r, cols[k]).font = BL
def drop(cid):
    r = find(CR, cid, end=450)
    for c in range(1, 9): CR.cell(r, c).value = None
claim('C-012', deriv='£23.0bn + £18.3bn = £41.3bn, shown as about £41bn')
claim('C-028', claim='10% biodiversity net gain mandatory since February 2024')
claim('C-040', deriv='Mortgages are 56.7% of Group exposure (CALC-52); flood is the largest-value theme; flood exposure of the book is not disclosed')
claim('C-042', claim='56.7%'); claim('C-044', claim='4.1%')
claim('C-049', claim='£120m to the Haweswater Aqueduct Resilience Programme', other='EV-064, ABB-HARP', deriv='Programme name spelled out (Glossary ABB-HARP, Built to Last PDF page 14)')
claim('C-070', claim='the gaps are targets and data', deriv='Gaps: absence checks C-076 (resilience lending volume), C-083 (water lending target), C-088 (nature metric for agriculture) and C-045 (flood exposure of the book)', other='C-076, C-083, C-088, C-045')
claim('C-071', claim='Built to Last report (2026)')
claim('C-072', claim='Underwrote £238.5m for Cleve Hill with NatWest')
claim('C-075', claim='its climate flag reflects home energy use')
claim('C-077', claim='Resilience and remediation lending for homes and property, such as flood retrofit and subsidence repair')
claim('C-079', claim='about 2.5m people', deriv='"approximately 2.5 million people"')
claim('C-084', claim='Finance drought resilience, storm overflows and water efficiency for growth sectors such as data centres')
claim('C-086', claim='grant-backed projects eligible', deriv='"eligible for environmentally based subsidies or grants" (shortened; no quotation marks on the slide)')
claim('C-087', claim='238,494 hectares', other='')
claim('C-090', claim='Nature-linked farm lending, with soil and water outcomes as key performance indicators for loans')
claim('C-107', claim='£bn, £m (bn and m are listed in the abbreviation key)')
claim('C-113', claim='about £60bn, indicative', deriv='£60.0bn, shown as about £60bn')
for cid in ('C-029', 'C-050', 'C-091'): drop(cid)
cr = max(x for x in range(5, 450) if CR.cell(x, 1).value) + 1
EXP = 'EV-040, EV-118, EV-119, EV-099, EV-068, EV-117, EV-045, EV-038, EV-100, EV-075, EV-039, EV-116'
NEW = [('C-147', 's3', 's3_title', '56.7%', 'Calculated', 'CALC-52', 'EV-040, EV-067', 'Mortgages £365,599m ÷ Group £645,058m'),
       ('C-150', 's3', 's3_exp_stat', '9.3%', 'Calculated', 'CALC-59', 'EV-034, EV-067', 'Nature priority sectors £59,991m ÷ Group £645,058m'),
       ('C-151', 's3', 's3_exp_sub', '£645.1bn', 'Calculated', 'CALC-51', 'EV-067', '£645,058m ÷ 1,000, 1 d.p.'),
       ('C-152', 's3', 's3_exp_sub', '31 December 2025', 'Quoted fact', 'EV-067', '', 'Tab 18 column header "31st December 2025"'),
       ('C-153', 's3', 's3_exp_sub', 'Share of total exposure by sector', 'Definition', 'DEF-EXPOSURE', '', 'Total exposure = lending + off-balance sheet'),
       ('C-154', 's3', 's3_exp_text', '£60.0bn', 'Quoted fact', 'EV-034', '', '£59,991m ÷ 1,000, 1 d.p.'),
       ('C-155', 's3', 's3_exp_text', 'sectors LBG classes as nature priorities', 'Definition', 'DEF-NATPRIOR', 'EV-034', 'LBG’s own classification (data sheet tab 18, footnote 12)'),
       ('C-156', 's3', 's3_exp_text', 'Mortgages count as climate risk, not nature', 'Quoted fact', 'EV-040', 'EV-069, EV-120', 'Tab 18: mortgages 56.7% of Group as increased climate risk, 0.0% as nature priority (EV-040); footnote 11 (EV-069, EV-120)'),
       ('C-157', 's3', 's3_exp_text', 'homeowners’ influence is low', 'Quoted fact', 'EV-120', '', 'Paraphrase, no quotation marks: "the direct influence of homeowners on the state of nature is considered to be low"'),
       ('C-158', 's3', 'auto:chart D', 'Book by sector: each sector total as % of the £645.1bn Group total; yellow = disclosed nature-priority %', 'Calculated', 'CALC-51', EXP, 'Deck_Chart A30:C41 = Client_Exposure sector totals ÷ Group total (QA 27: sector totals add up to the Group total); nature-priority % copied from tab 18 (Client_Exposure K)'),
       ('C-159', 's1', 'auto:abbreviation keys', 'Abbreviation key at the foot of pages 2–4', 'Definition', 'ABB-NBSAP', 'ABB-EY, ABB-ENCORE, ABB-LBG, ABB-UK, ABB-PRA, ABB-SS525, ABB-AI, ABB-BN, ABB-M', 'Built by Excel from Glossary column G (key text) for every abbreviation found on the page (Glossary I:K)')]
for row in NEW: setrow(CR, cr, list(row)); cr += 1
wb.save(OUT); print('saved', OUT)
