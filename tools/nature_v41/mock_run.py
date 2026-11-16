"""Mock test of the Copilot run (v5.0), prompt by prompt, from the BLANK master template.
1. Writes the expected agent output for each prompt (exact headers) to OUTDIR/Pxx_*.md, taken from the reference LBG workbook.
2. Pastes each output into a copy of the blank master ONLY at the locations the playbook documents.
Then (run separately) recalc the result and compare it with the reference.  Usage: python mock_run.py MASTER.xlsx LBG_REF.xlsx OUTDIR OUT.xlsx"""
import sys, os, openpyxl
MASTER, REF, OUTDIR, OUT = sys.argv[1:5]
os.makedirs(OUTDIR, exist_ok=True)
ref = openpyxl.load_workbook(REF)            # formulas (for Calculations)
refv = openpyxl.load_workbook(REF, data_only=True)
wb = openpyxl.load_workbook(MASTER)
def rows(ws, c1, c2, start=5, key=1, end=500):
    return [[ws.cell(r, c).value for c in range(c1, c2 + 1)] for r in range(start, end) if ws.cell(r, key).value not in (None, '')]
def md(name, title, blocks, note=''):
    out = [f'# {title}', '', note, ''] if note else [f'# {title}', '']
    for head, data in blocks:
        out.append('| ' + ' | '.join(str(h) for h in head) + ' |'); out.append('|' + '---|' * len(head))
        for r in data: out.append('| ' + ' | '.join('' if v is None else str(v).replace('|', '¦').replace('\n', ' ') for v in r) + ' |')
        out.append('')
    open(os.path.join(OUTDIR, name), 'w').write('\n'.join(out)); return sum(len(d) for _, d in blocks)
def paste(ws, r0, c0, data):
    for i, row in enumerate(data):
        for j, v in enumerate(row):
            if v is not None: ws.cell(r0 + i, c0 + j).value = v
def clear(ws, r, cols):
    for c in cols: ws.cell(r, c).value = None
hdr = lambda ws, c1, c2, r=4: [ws.cell(r, c).value for c in range(c1, c2 + 1)]
log = []
# ---- Prompt 0: inputs
I = ref['Inputs']; inp = [[I.cell(r, 1).value, I.cell(r, 2).value] for r in range(5, 17) if not str(I.cell(r, 2).value).startswith('=')]
for r in range(5, 17):
    if not str(I.cell(r, 2).value).startswith('='): wb['Inputs'].cell(r, 2).value = I.cell(r, 2).value
core = [['(1) Annual report and/or sustainability report', 'Missing – not attached (upload failed). Record as Included = N; keep the page 4 caveat.'],
        ['(2) Sustainability / ESG data sheet', 'Attached: 2025-lbg-sustainability-data-sheet.xlsx'],
        ['(3) Sustainable / green financing framework', 'Attached: 2025-lbg-sustainable-financing-framework.pdf'],
        ['(4) Nature, TNFD or climate report', 'Attached: working-with-nature-report.pdf; lbg-built-to-last-report.pdf']]
log.append(('0', md('P0_inputs.md', 'Prompt 0 – inputs confirmed and core sources checked', [(['Field', 'Value'], inp), (['Core source', 'Status'], core)])))
# ---- Prompt 1: sources (paste from row 7)
S = ref['Sources']; src = [r for r in rows(S, 1, 10) if r[0] not in ('SRC-EY', 'SRC-METH')]
for r in (7, 8): clear(wb['Sources'], r, range(1, 11))
paste(wb['Sources'], 7, 1, src)
log.append(('1', md('P1_sources.md', 'Prompt 1 – Sources (paste at Sources!A7)', [(hdr(S, 1, 10), src)])))
# ---- evidence split by prompt
E = ref['Evidence_Log']; ev = rows(E, 1, 9)
expo_ids = {r[0] for r in rows(ref['Client_Exposure'], 1, 1)}
growth_src = {'SRC-07', 'SRC-08', 'SRC-09', 'SRC-10'}
p2 = [r for r in ev if r[0] not in expo_ids and r[1] not in growth_src]
p3e = [r for r in ev if r[0] in expo_ids]
p3b = [r for r in ev if r[1] in growth_src and r[0] not in expo_ids]
assert len(p2) + len(p3e) + len(p3b) == len(ev)
clear(wb['Evidence_Log'], 5, range(1, 10))
paste(wb['Evidence_Log'], 5, 1, p2 + p3e + p3b)
for s_id in sorted({r[1] for r in p2}):
    sub = [r for r in p2 if r[1] == s_id]
    log.append((f'2 ({s_id})', md(f'P2_evidence_{s_id}.md', f'Prompt 2 – Evidence_Log rows for {s_id} (paste at the next empty Evidence_Log row)', [(hdr(E, 1, 9), sub)])))
CE = ref['Client_Exposure']; ce = rows(CE, 1, 12)
clear(wb['Client_Exposure'], 5, range(1, 13)); paste(wb['Client_Exposure'], 5, 1, ce)
log.append(('3', md('P3_exposure.md', 'Prompt 3 – Client_Exposure (paste at A5) and matching Evidence_Log rows', [(hdr(CE, 1, 12), ce), (hdr(E, 1, 9), p3e)],
                    'Columns K and L copy the disclosed % of Group for each sector total (data sheet tab 18). Column M is a formula: do not paste over it.')))
GS = ref['Growth_Sectors']; gs = rows(GS, 1, 8)
paste(wb['Growth_Sectors'], 5, 1, gs)
log.append(('3b', md('P3b_growth.md', 'Prompt 3b – growth priorities: Evidence_Log rows, then Growth_Sectors (paste at A5)', [(hdr(E, 1, 9), p3b), (hdr(GS, 1, 8), gs)])))
# ---- Prompt 4: mapping + classification review
M = ref['NBSAP_Mapping']; mp = [[r[0]] + r[2:8] for r in rows(M, 1, 8)]
clear(wb['NBSAP_Mapping'], 5, [1, 3, 4, 5, 6, 7, 8])
for i, r in enumerate(mp):
    wb['NBSAP_Mapping'].cell(5 + i, 1).value = r[0]; paste(wb['NBSAP_Mapping'], 5 + i, 3, [r[1:]])
NC = ref['NBSAP_Commitments']; ov = [[NC.cell(r, 1).value, NC.cell(r, 7).value, NC.cell(r, 14).value, NC.cell(r, 15).value] for r in range(5, 1000) if NC.cell(r, 14).value]
mrow = {wb['NBSAP_Commitments'].cell(r, 1).value: r for r in range(5, 1000) if wb['NBSAP_Commitments'].cell(r, 1).value}
for ex, _, n, o in ov: wb['NBSAP_Commitments'].cell(mrow[ex], 14).value = n; wb['NBSAP_Commitments'].cell(mrow[ex], 15).value = o
log.append(('4', md('P4_mapping.md', 'Prompt 4 – NBSAP_Mapping (paste A and C:H from row 5) and classification review (NBSAP_Commitments N:O)',
                    [([hdr(M, 1, 1)[0]] + hdr(M, 3, 8), mp), (['Example_ID', 'Current SubTheme_ID', 'Proposed SubTheme_ID', 'Rationale (EY view)'], ov)])))
# ---- Prompt 5, 5b
SC = ref['Scoring']; sc = [[SC.cell(r, c).value for c in range(23, 30)] for r in range(5, 20)]
paste(wb['Scoring'], 5, 23, sc)
log.append(('5', md('P5_scores.md', 'Prompt 5 – judgement scores (paste at Scoring!W5)', [(['SubTheme_ID', 'C1', 'C2', 'C4', 'Evidence_IDs', 'Rationale', 'Client exposure (as disclosed)'], sc)])))
TV = ref['Theme_View']; al = [[TV.cell(r, 1).value, TV.cell(r, 20).value, TV.cell(r, 21).value] for r in range(5, 10)]
for i, r in enumerate(al): wb['Theme_View'].cell(5 + i, 20).value = r[1]; wb['Theme_View'].cell(5 + i, 21).value = r[2]
log.append(('5b', md('P5b_alignment.md', 'Prompt 5b – strategy alignment (paste T:U, rows 5–9 in order W, AFF, UD, DM, P)', [(['Theme_ID', 'In client’s stated priorities? (Yes/No)', 'Evidence_IDs'], al)])))
# ---- Prompt 6
G = ref['Glossary']; gl = rows(G, 1, 7, start=60)
paste(wb['Glossary'], 60, 1, gl)
C = ref['Calculations']; ca = rows(C, 1, 7, start=30)
paste(wb['Calculations'], 30, 1, ca)
log.append(('6', md('P6_glossary_calcs.md', 'Prompt 6 – Glossary rows (paste from row 60, columns A:G; G = Key text) and Calculations (paste from row 30)', [(hdr(G, 1, 7), gl), (hdr(C, 1, 7), ca)])))
# ---- Prompt 7
SCn = ref['Slide_Content']; st = [[SCn.cell(r, c).value for c in (10, 11, 12)] for r in range(5, 121) if SCn.cell(r, 10).value]
paste(wb['Slide_Content'], 5, 10, st)
CR = ref['Claims_Register']; cl = rows(CR, 1, 8)
paste(wb['Claims_Register'], 5, 1, cl)
log.append(('7', md('P7_slides_claims.md', 'Prompt 7 – (a) Slide_Content (paste at J5) and (b) Claims_Register (paste at A5)', [(['Key', 'Value', 'Claim_IDs'], st), (hdr(CR, 1, 8), cl)])))
wb.save(OUT)
print('pasted into', OUT); [print(f'  Prompt {p}: {n} rows') for p, n in log]
