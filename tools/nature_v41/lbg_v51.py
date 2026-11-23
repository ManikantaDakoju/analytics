"""v5.1 LBG example: the reviewed wording (19 November) plus Helen's updates, all traced.
Usage: python lbg_v51.py LBG_v5.0.xlsx OUT.xlsx"""
import openpyxl, sys
from openpyxl.styles import Font
from patch_v51 import patch51
import verify_quotes as V
SRC, OUT = sys.argv[1:3]
wb = openpyxl.load_workbook(SRC); patch51(wb)
BL = Font(name='Arial', size=10, color='0000FF'); TODAY = '23/11/2026'
def setrow(ws, r, vals, c0=1):
    for j, v in enumerate(vals, c0):
        if v is not None: x = ws.cell(r, j); x.value = v; x.font = BL
def nextrow(ws, col=1, start=5):
    r = start
    while ws.cell(r, col).value not in (None, ''): r += 1
    return r
def find(ws, val, col=1, start=5, end=500):
    return next(r for r in range(start, end) if ws.cell(r, col).value == val)
wb['Inputs']['B10'] = 'Client discussion (presented)'; wb['Inputs']['B11'] = TODAY

# ---- evidence: sustainability-linked loans already measure borrower KPIs (financing framework, printed page 24 = PDF 25)
q = 'aligning the loan economic and structural terms to the borrower’s sustainability performance, which is measured using one or more sustainability KPIs'
assert V.check('SRC-05', 25, q) == 'exact', V.check('SRC-05', 25, q)
E = wb['Evidence_Log']; M = wb['NBSAP_Mapping']; r = nextrow(E)
setrow(E, r, ['EV-121', 'SRC-05', 'p.24', '25', q, 'Financing or product', 'Products and services',
              'Sustainability-linked loans with borrower KPIs already exist in the framework (any sector, including farming).', f'Script check vs PDF text: exact ({TODAY})'])
m = nextrow(M); tpl = find(M, 'EV-100')
for c in range(1, 10):
    v = M.cell(tpl, c).value
    M.cell(m, c).value = v.replace(f'{tpl}', f'{m}').replace(f'{tpl-1}', f'{m-1}') if isinstance(v, str) and v.startswith('=') else v
for c, v in ((1, 'EV-121'), (3, 'Client action'), (4, 'AFF02'), (5, None), (6, None), (7, 'Indirect'), (8, 'Sustainability-linked loan structure usable for nature-linked farm lending')):
    M.cell(m, c).value = v

# ---- slide text (paste area J:L) with claim lists
s = wb['Slide_Content']; rows = {s.cell(x, 10).value: x for x in range(5, 121) if s.cell(x, 10).value}
lim = {s.cell(x, 1).value: s.cell(x, 5).value for x in range(5, 200) if s.cell(x, 1).value}
def put(k, v=None, cl=None):
    if k not in rows: rows[k] = nextrow(s, 10)
    x = rows[k]; s.cell(x, 10).value = k; s.cell(x, 10).font = BL
    if v is not None:
        assert len(v) <= (lim.get(k) or 9999), (k, len(v), lim.get(k)); s.cell(x, 11).value = v; s.cell(x, 11).font = BL
    if cl is not None: s.cell(x, 12).value = cl; s.cell(x, 12).font = BL
for old in ('s2_grow1_head', 's2_grow1_text', 's2_grow2_head', 's2_grow2_text', 's2_grow3_head', 's2_grow3_text', 's2_grow4_head', 's2_grow4_text'):
    if old in rows:
        for c in (10, 11, 12): s.cell(rows[old], c).value = None
        del rows[old]
T = {
 'cover_date': ('For discussion – 10 December 2026', ''),
 's1_title': ('UK nature investment and future demand centre on water and flood', 'C-002, C-010, C-011'),
 's1_ctx_why': ('51% of UK economic output is highly dependent on nature, while LBG estimates a £56bn UK nature finance gap.', 'C-120, C-001, C-117'),
 's1_callout_text': ('of identified commitments are directed to water and flood (about £41bn)', 'C-012, C-013'),
 's1_sub_r': ('Growth, water scarcity and regulation are increasing the need for investment', 'C-020'),
 's1_tile2_text': ('people under water-use restrictions during the 2026 drought; reservoirs fell to 69%', 'C-023, C-024'),
 's1_tile3_stat': ('5bn litres/day', 'C-025'),
 's1_tile3_text': ('Projected public water shortfall by 2055 without action', 'C-025'),
 's1_tile4_text': ('Housing, net zero, food and data centres all require water', 'C-027'),
 's1_rules_lead': ('Regulatory expectations are also increasing', ''),
 's1_rules': ('10% biodiversity net gain is mandatory, while PRA SS5/25 raises expectations for banks on physical and transition climate risk.', 'C-028, C-030, C-031, C-032'),
 's1_sowhat': ('Growth in water-dependent sectors will increase the need for resilient infrastructure, creating a growing role for financing water and flood resilience.', 'C-034, C-119'),
 's2_title': ('Three priority themes take 81% of UK nature commitments', 'C-161, C-160, C-002'),
 's2_section': ('Translating UK investment priorities to LBG’s portfolio', ''),
 's2_lead': ('Water and flood represent 69% of identified UK nature commitments; the table maps these and the other nature themes to LBG’s portfolio.', 'C-010, C-002, C-001'),
 's2_link_DM': ('Mortgages £365.6bn (56.7%) and property £26.3bn (4.1%): a potential channel for flood risk. The flood-exposed share of the mortgage book is not disclosed.', 'C-041, C-042, C-043, C-044, C-045, C-046'),
 's2_link_W': ('Energy and water supply £12.1bn (1.9%); direct financing link through £120m for the Haweswater Aqueduct Resilience Programme.', 'C-047, C-048, C-049'),
 's2_link_AFF': ('Agriculture, forestry and fishing £6.8bn (1.1%); direct customer link through LBG’s soil partnership with 685 farms.', 'C-051, C-052, C-053, C-054'),
 's2_link_UD': ('Construction £5.9bn (0.9%), including £2.3bn housebuilders; direct link through development and land-use pressures.', 'C-055, C-056, C-057, C-162'),
 's2_link_P': ('Manufacturing £13.1bn (2.0%); indirect link through pollution, waste and resource-use pressures.', 'C-058, C-059, C-060'),
 's2_other1_head': ('Urban development', 'C-105'),
 's2_other1_text': ('£5.9bn construction lending (a nature-priority sector for LBG) and mandatory biodiversity net gain point to net gain finance for housebuilders.', 'C-055, C-155, C-028, C-163'),
 's2_other2_head': ('Pollution and waste', 'C-105'),
 's2_other2_text': ('18% of UK commitments (£10.8bn) but a weak link in LBG’s book: scope to grow waste and circular-economy finance under its existing framework.', 'C-164, C-165, C-166, C-002'),
 's2_sowhat': ('69% of identified UK commitments target water and flood. For LBG, the link goes beyond its 1.9% direct energy and water exposure to flood risk across the £365.6bn mortgage book.', 'C-010, C-048, C-061, C-041, C-046'),
 's3_title': ('Nature-priority sectors make up 9.3% (£60.0bn) of LBG’s book', 'C-155, C-147, C-154, C-001'),
 's3_section': ('Portfolio exposure, current actions and areas to explore', ''),
 's3_exp_stat': ('9.3% | £60.0bn', 'C-150, C-154'),
 's3_exp_sub': ('Share of £645.1bn total exposure by sector, as at 31 December 2025', 'C-151, C-152, C-153'),
 's3_exp_text': ('Exposure in sectors identified by LBG as nature priorities', 'C-155, C-001'),
 's3_exp_note': ('LBG classes mortgages as 0% nature priority, although they carry physical risks such as flooding.', 'C-156, C-001'),
 's3_col1_today': ('Underwrote £238.5m, alongside NatWest, for Cleve Hill, including coastal flood-defence measures. Flood doors and barriers are eligible under LBG’s sustainable financing framework.', 'C-072, C-073, C-074'),
 's3_col1_gap': ('What proportion of the £365.6bn mortgage book is exposed to flood risk? How much lending currently supports property-level flood resilience?', 'C-045, C-041, C-076'),
 's3_col1_opp': ('Scale resilience and remediation finance for homes and property, including flood-resilience retrofits and subsidence repair.', 'C-077'),
 's3_col2_today': ('Provided £120m of debt to the approximately £3bn Haweswater Aqueduct Resilience Programme, supporting water supply for around 2.5 million people. Wastewater treatment, reservoirs and wetlands are also eligible.', 'C-049, C-078, C-079, C-080, C-081, C-082'),
 's3_col2_gap': ('What water-specific lending targets and volumes are in place? How much of the £12.1bn energy and water supply exposure relates specifically to water?', 'C-083, C-047'),
 's3_col2_opp': ('Scale finance for drought resilience, storm-overflow reduction and water efficiency, including solutions for water-dependent growth sectors such as data centres.', 'C-084'),
 's3_col3_today': ('Ten-year Soil Association Exchange partnership covering 685 farms and 238,494 hectares. Regenerative agriculture and grant-supported projects are eligible; sustainability-linked loans already use borrower indicators.', 'C-054, C-053, C-087, C-085, C-086, C-168'),
 's3_col3_gap': ('Which nature-related metrics could complement the existing emissions target across the £6.8bn agriculture portfolio?', 'C-088, C-051, C-089'),
 's3_col3_opp': ('Link Soil Association Exchange farm scorecards to LBG’s existing sustainability-linked loans, and report the lending volumes.', 'C-090'),
 's3_sowhat': ('LBG has enabling frameworks, partnerships and flagship transactions in place. For water and flood, the next step is to set portfolio targets, scale resilience finance and strengthen exposure data.', 'C-092, C-070'),
 's3_footnote': ('Sources: LBG Working with nature (2024) page 4; Built to Last (2026) pages 14–17; Sustainability Data Sheet 2025, tabs 18 and 20; Sustainable Financing Framework 2025 pages 11–15, 24. "Areas to explore" = not found in these four documents; to be checked against the Annual and Sustainability Reports 2025.', ''),
 's2_talk': ('Same five themes, now set against LBG. The title says it: the top three themes take 81% of identified UK commitments. Read each row across: LBG’s relevance score, the UK money, and where LBG’s book touches it. A tick in the UK priority column means the theme takes at least 10% of UK commitments; the tag under a theme name means LBG itself names it as a priority. Water and flood lead, but LBG’s direct energy and water lending is small (1.9% of the book); the bigger link is indirect, through flood risk across the £365.6bn mortgage book. The strip at the bottom covers the other two themes: urban development, where mandatory biodiversity net gain creates demand for finance from housebuilders, and pollution and waste, which takes 18% of UK commitments but has only a weak link in LBG’s book.', ''),
 's3_talk': ('Start with LBG’s own classification: 9.3% of its book, £60.0bn, sits in sectors it calls nature priorities. Mortgages are 56.7% of the book, but LBG classes them as 0% nature priority because homeowners’ direct influence on nature is low; they matter here for physical risk such as flooding. Then credit LBG: Cleve Hill, the Haweswater Aqueduct Resilience Programme, the Soil Association partnership and a financing framework that already covers water, flood resilience, regenerative farming and sustainability-linked loans. Then the areas to explore, which are things not found in the public documents, not criticisms. Then the opportunities. To make this LBG-specific, the next steps are LBG’s own lending data by sector, region and product, then property locations against flood and drought maps.', ''),
}
for k, (v, cl) in T.items(): put(k, v, cl if cl else None)

# ---- claims
CR = wb['Claims_Register']
def claim(cid, **kw):
    r = find(CR, cid, end=450); cols = dict(claim=4, type=5, primary=6, other=7, deriv=8, key=3)
    for k, v in kw.items(): CR.cell(r, cols[k]).value = v; CR.cell(r, cols[k]).font = BL
def drop(cid):
    try: r = find(CR, cid, end=450)
    except StopIteration: return
    for c in range(1, 9): CR.cell(r, c).value = None
for cid in ['C-%03d' % n for n in range(130, 146)] + ['C-021' if False else 'C-026', 'C-033', 'C-040', 'C-071', 'C-075', 'C-121', 'C-157']: drop(cid)
claim('C-020', claim='Growth, water scarcity and regulation are increasing the need for investment')
claim('C-028', claim='10% biodiversity net gain is mandatory')
claim('C-030', claim='PRA SS5/25 raises expectations for banks on physical and transition climate risk', deriv='Source: "raising expectations for how UK banks and insurers identify and manage physical climate and transition risks" (corrected after the 19 November review: not physical risk only)')
claim('C-034', claim='Growth in water-dependent sectors will increase the need for resilient infrastructure', type='EY view', primary='EV-080', other='EV-081, EV-095, EV-079', deriv='Government: growth cannot happen without water (EV-080); projected shortfall (EV-081); developments not held back by water (EV-095)')
claim('C-046', claim='a potential channel for flood risk', deriv='EY view: physical flood risk to mortgaged homes and property; flood exposure of the book not disclosed (C-045)')
claim('C-060', claim='indirect link through pollution, waste and resource-use pressures')
claim('C-077', claim='Scale resilience and remediation finance for homes and property, including flood-resilience retrofits and subsidence repair')
claim('C-079', claim='around 2.5 million people')
claim('C-084', claim='Scale finance for drought resilience, storm-overflow reduction and water efficiency, including solutions for water-dependent growth sectors such as data centres')
claim('C-086', claim='grant-supported projects are eligible')
claim('C-090', claim='Link Soil Association Exchange farm scorecards to LBG’s existing sustainability-linked loans, and report the lending volumes',
      primary='EV-121', other='EV-001, EV-002, EV-047', deriv='Reworded after the 19 November review: the framework already offers sustainability-linked loans with borrower KPIs (EV-121) and soil-health eligibility (EV-047), so the opportunity is to connect the existing partnership scorecards (EV-001, EV-002) to that product and disclose volumes')
claim('C-147', key='s3_title', claim='9.3%', type='Calculated', primary='CALC-59', other='EV-034, EV-067', deriv='Nature priority sectors £59,991m ÷ Group £645,058m')
claim('C-156', claim='LBG classes mortgages as 0% nature priority, although they carry physical risks such as flooding', key='s3_exp_note',
      deriv='Tab 18: mortgages 0.0% nature priority (EV-040); footnote 11 (EV-120). "Physical risks such as flooding" = EY view (C-045, C-046)')
claim('C-070', key='s3_sowhat', claim='the next step is to set portfolio targets, scale resilience finance and strengthen exposure data')
claim('C-092', claim='LBG has enabling frameworks, partnerships and flagship transactions in place')
cr = max(x for x in range(5, 450) if CR.cell(x, 1).value) + 1
NEW = [('C-160', 's2', 's2_title', '81%', 'Calculated', 'CALC-TOP3-S', 'CALC-S-DM, CALC-S-W, CALC-S-AFF', '38.3% + 30.5% + 12.0% = 80.7%, shown as 81% (top three themes by score)'),
       ('C-161', 's2', 's2_title', 'Three priority themes', 'Definition', 'DEF-SCORE', 'CALC-TH-DM, CALC-TH-W, CALC-TH-AFF', 'The three highest-scoring themes (4.6, 4.5, 4.2)'),
       ('C-162', 's2', 's2_link_UD', 'direct link through development and land-use pressures', 'EY view', 'EV-039', 'EV-076, EV-005', 'Construction and housebuilding drive land-use change; biodiversity net gain applies to new developments (EV-005)'),
       ('C-163', 's2', 's2_other1_text', 'point to net gain finance for housebuilders', 'EY view', 'EV-005', 'EV-039, EV-076, EV-024', 'Mandatory 10% net gain for new developments (EV-005); £2.3bn housebuilder lending (EV-076); LBG already finances a project with a net gain commitment (EV-024)'),
       ('C-164', 's2', 's2_other2_text', '18%', 'Calculated', 'CALC-S-P', '', 'Pollution and waste share of UK commitments'),
       ('C-165', 's2', 's2_other2_text', '£10.8bn', 'Calculated', 'CALC-V-P', '', ''),
       ('C-166', 's2', 's2_other2_text', 'a weak link in LBG’s book: scope to grow waste and circular-economy finance under its existing framework', 'EY view', 'EV-060', 'EV-044, SC-P02', 'Theme score 3.2 (lowest; materiality to LBG 2); framework already has circular-economy criteria (EV-060); LBG flags waste management as nature-relevant (EV-044)'),
       ('C-167', 's2', 'auto:r1-r5 UK tick', '✓ = theme takes 10% or more of identified UK commitments (flood 38%, water 31%, pollution 18%, agriculture 12%)', 'Definition', 'DEF-UKPRIO', 'CALC-S-DM, CALC-S-W, CALC-S-P, CALC-S-AFF, CALC-S-UD', 'Theme_View X5:X9; threshold Theme_View X2'),
       ('C-168', 's3', 's3_col3_today', 'sustainability-linked loans already use borrower indicators', 'Quoted fact', 'EV-121', '', 'Paraphrase, no quotation marks (financing framework page 24)')]
for row in NEW: setrow(CR, cr, list(row)); cr += 1
wb.save(OUT); print('saved', OUT)
