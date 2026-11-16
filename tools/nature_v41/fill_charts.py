"""Do what Prompt 9c asks the user to do by hand (Edit Data > paste): load Deck_Chart ranges into the four charts.
Usage: python fill_charts.py DECK.pptx WB.xlsx OUT.pptx"""
import sys, openpyxl
from pptx import Presentation
from pptx.chart.data import CategoryChartData
DECK, WB, OUT = sys.argv[1:4]
dc = openpyxl.load_workbook(WB, data_only=True)['Deck_Chart']
RANGES = {'chart-A-money-by-theme': (5, 6, 10), 'chart-B-score': (13, 14, 18), 'chart-C-money': (21, 22, 26), 'chart-D-book': (29, 30, 41)}
p = Presentation(DECK); done = []
for sl in p.slides:
    for sh in sl.shapes:
        if sh.has_chart and sh.name in RANGES:
            h, a, b = RANGES[sh.name]
            rows = [[dc.cell(r, c).value for c in (1, 2, 3)] for r in range(a, b + 1)]
            rows = [r for r in rows if r[0] not in (None, '')]          # 'delete empty rows after pasting'
            cd = CategoryChartData(); cd.categories = [r[0] for r in rows]
            for j in (1, 2): cd.add_series(dc.cell(h, j + 1).value, [float(r[j] or 0) for r in rows])
            sh.chart.replace_data(cd); done.append((sh.name, len(rows)))
p.save(OUT); print('charts filled:', done)
