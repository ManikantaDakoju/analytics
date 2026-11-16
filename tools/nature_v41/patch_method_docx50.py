"""Update the plain-English methodology .docx to v5.0. Usage: python patch_method_docx50.py IN.docx OUT.docx"""
import docx, sys
d = docx.Document(sys.argv[1])
def paras():
    yield from d.paragraphs
    for t in d.tables:
        for row in t.rows:
            for c in row.cells: yield from c.paragraphs
def rep(a, b):
    n = 0
    for p in paras():
        if a not in p.text: continue
        hit = False
        for r in p.runs:
            if a in r.text: r.text = r.text.replace(a, b); hit = True; n += 1
        if not hit:                      # text split across runs: rewrite in the first run, keep its format
            full = p.text.replace(a, b)
            for i, r in enumerate(p.runs): r.text = full if i == 0 else ''
            n += 1
    return n
CH = [('4.5 (growth-vs-book view and strategy alignment, 9 November 2026)', '5.0 (abbreviation key, portfolio exposure panel and mock test, 16 November 2026)'),
      ('9 November 2026', '16 November 2026'),
      ('2. Context', '2. Context setting'),
      ('3. Context × client', '3. National nature finance and the client'),
      ('4. Client', '4. Client portfolio exposure and position'),
      ('For the top three themes: what the client does today', 'The client’s book by sector and its nature-priority share; then, for the top three themes, what the client does today'),
      ('5. EY', '5. Why EY'),
      ('How EY can support. Written by the EY team, not by the agent.', 'Why EY: headings and placeholders only, written by the EY team, not by the agent.'),
      ('Example (LBG, page 3: “57%” of the book is mortgages)', 'Example (LBG, page 3: “56.7%” of the book is mortgages)'),
      ('“Mortgages £365.6bn (57%) …”', '“Mortgages £365.6bn (56.7%) …”'),
      ('C-042 “57%”', 'C-042 “56.7%”'),
      ('Value check confirms that 57% on the slide', 'Value check confirms that 56.7% on the slide'),
      ('Mortgages £365.6bn (57%); flood exposure not disclosed', 'Mortgages £365.6bn (56.7%); flood exposure not disclosed'),
      ('£120m to HARP', '£120m to the Haweswater Aqueduct Resilience Programme'),
      ('£120m of debt to HARP;', '£120m of debt to the Haweswater Aqueduct Resilience Programme;'),
      ('the three charts are refreshed with Edit Data', 'the four charts are refreshed with Edit Data'),
      ('Page 5 data ladder: public data → portfolio data → asset level', 'Page 4: the book-by-sector panel shows what public data can say; the “So what” and talk track set out the next steps (portfolio data, then asset level). Page 5 is left to the EY team.'),
      ('Abbreviations defined', 'Abbreviations defined and keyed'),
      ('Glossary entries with sources; QA check that each abbreviation used is cited', 'Words written in full where space allows; Glossary entries with sources; every abbreviation left on a page is listed in the key at its foot (QA 24–25)'),
      ('LBG_UK_Nature_Mapping_v4.xlsx and LBG_Nature_Opportunities_v4.pptx', 'LBG_UK_Nature_Mapping_v5.0.xlsx and LBG_Nature_Opportunities_v5.0.pptx'),
      ('Cover + 4 pages with placeholders and charts', 'Cover + 4 pages with placeholders, four charts and an abbreviation key on pages 2–4')]
for a, b in CH: print(rep(a, b), 'x', a[:50])
for p in d.paragraphs:
    t = p.text
    if t.startswith('Automatic checks (QA_Log):'):
        for i, r in enumerate(p.runs): r.text = '' if i else ''
        p.runs[0].text = ('Automatic checks (QA_Log): 27 checks, including 6 for traceability, 1 for growth-sector evidence, 1 confirming the client’s annual / sustainability report '
                          'was reviewed, 1 for strategy-alignment evidence, 2 for the abbreviation key, 1 for mapping strength and 1 logic check that the client’s sector totals add up to its Group total. '
                          'Every slide value must have valid claims, every claim must resolve to a source, and every calculated figure must equal its formula.')
    if t.startswith('Evidence base:'):
        for i, r in enumerate(p.runs): r.text = ''
        p.runs[0].text = ('Evidence base: 119 evidence items: all 83 PDF quotes were checked word for word against the PDF page, the 24 data-sheet figures and notes against the spreadsheet, '
                          'and the 12 ENCORE figures were extracted directly from the CSV rows. There are 110 claims. 26 of 27 QA checks pass; the other is a Warning because the '
                          'LBG Sustainability Report 2025 could not be reviewed, and page 4 says so. A mock run from the blank template reproduced the deck exactly.')
    if t.startswith('Step 7: Fill the EY template'): pass
d.save(sys.argv[2]); print('saved')
# add v5.0 rows to the "What changed" table
import copy
d = docx.Document(sys.argv[2])
for t in d.tables:
    if t.rows[0].cells[0].text.strip() == 'Feedback':
        for a, b in (('Use full forms; explain any abbreviation', 'Words written in full on the slides; an abbreviation key at the foot of pages 2–4, built automatically from the Glossary'),
                     ('Show the client’s portfolio exposure', 'Page 4 now opens with the client’s book by sector (chart D), its nature-priority share and how the largest sector is classified'),
                     ('Keep the “Why EY” page for the EY team', 'Page 5 (Why EY) holds headings and placeholders only')):
            new = copy.deepcopy(t.rows[-1]._tr); t._tbl.append(new)
            row = t.rows[-1]
            for cell, txt in zip(row.cells, (a, b)):
                ps = cell.paragraphs
                for p in ps[1:]: p._p.getparent().remove(p._p)
                runs = ps[0].runs
                for i, r in enumerate(runs): r.text = txt if i == 0 else ''
d.save(sys.argv[2]); print('table rows added')
