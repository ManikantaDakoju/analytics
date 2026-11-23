"""Mock-test verdict: the deck and workbook rebuilt from the BLANK master via the documented paste locations must match the reference."""
import sys, re, openpyxl
from pptx import Presentation
W = sys.argv[1]; V = sys.argv[2] if len(sys.argv) > 2 else '50'
def dump(f):
    p = Presentation(f); out = []
    for i, s in enumerate(p.slides, 1):
        for sh in s.shapes:
            for x in (list(sh.shapes) if sh.shape_type == 6 else [sh]):
                if x.has_text_frame: out.append((i, x.name, x.text_frame.text))
            if sh.has_chart: out.append((i, sh.name, str([(list(pl.categories), [list(se.values) for se in pl.series]) for pl in sh.chart.plots])))
        out.append((i, 'notes', s.notes_slide.notes_text_frame.text if s.has_notes_slide else ''))
    return out
a, b = dump(f'{W}/mock_deck.pptx'), dump(f'{W}/LBG_deck_v{V}.pptx')
qa = lambda f: [(r[0], r[2]) for r in openpyxl.load_workbook(f, data_only=True)['QA_Log'].iter_rows(min_row=5, values_only=True) if r[0]]
qm, qr = qa(f'{W}/mock_from_blank.xlsx'), qa(f'{W}/LBG_v{V}.xlsx')
res = {'deck identical (text, charts, notes)': a == b, 'QA identical': qm == qr,
       'no {{placeholders}} left': not [t for t in a if '{{' in t[2]]}
for pg in sorted({t[0] for t in a if t[1] == 'abbreviation-key' and not t[2].startswith('[')}):
    txt = ' '.join(t[2] for t in a if t[0] == pg and t[1] not in ('notes', 'abbreviation-key') and not t[1].startswith('chart'))
    key = next(t[2] for t in a if t[0] == pg and t[1] == 'abbreviation-key')
    caps = sorted(set(re.findall(r'\b[A-Z][A-Z0-9/]{1,}\b', txt)))
    short = sorted(set(re.findall(r'\b(?:e\.g|i\.e|incl|approx|vs|etc|pp|ha|Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\b|\bc\.', txt)))
    res[f'page {pg}: abbreviations {caps} all in key'] = all(c + ' =' in key for c in caps)
    res[f'page {pg}: no other short forms'] = not short
for k, v in res.items(): print(('PASS ' if v else 'FAIL ') + k)
print('QA not Pass:', [x for x in qm if x[1] != 'Pass'])
sys.exit(0 if all(res.values()) else 1)
