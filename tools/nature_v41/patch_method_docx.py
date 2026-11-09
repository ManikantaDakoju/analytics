"""Update the plain-English methodology .docx to v4.1 (growth step, context band, paste areas, counts)."""
import docx, sys, copy
d=docx.Document(sys.argv[1])
def rep_all(a,b):
    n=0
    for p in d.paragraphs:
        if a in p.text:
            for r in p.runs:
                if a in r.text: r.text=r.text.replace(a,b); n+=1
    for t in d.tables:
        for row in t.rows:
            for c in row.cells:
                for p in c.paragraphs:
                    for r in p.runs:
                        if a in r.text: r.text=r.text.replace(a,b); n+=1
    return n
changes=[('4.0 (after partner review, 22 October 2026)','4.1 (test run and growth step, 9 November 2026)'),('5 November 2026','9 November 2026'),
('76 evidence items; all 59 PDF quotes were checked word for word against the PDF page and the 17 data-sheet figures against the spreadsheet. There are 76 claims, and all 19 QA checks pass.',
 '88 evidence items; all 71 PDF quotes were checked word for word against the PDF page and the 17 data-sheet figures against the spreadsheet. There are 83 claims, and all 21 QA checks pass.'),
('19 checks, including 6 for traceability.','21 checks, including 6 for traceability and 1 for growth-sector evidence.'),
('follow Prompts 0–9.','follow Prompts 0–9 (with 3b once per country).'),
('Page 2: infrastructure pipeline, drought, growth sectors (data centres, housing), rules','Page 2: context band, infrastructure pipeline, drought, the Environment Agency’s water shortfall, and growth sectors that “require water” (government sources), rules')]
for a,b in changes: print(rep_all(a,b),'x',a[:40])
# add the growth step after Step 1
for i,p in enumerate(d.paragraphs):
    if p.text.startswith('Step 2: Extract the evidence'):
        h=copy.deepcopy(p._p); p._p.addprevious(h)
        new_h=docx.text.paragraph.Paragraph(h,p._parent); new_h.runs[0].text='Step 1b: National growth priorities (once per country)'
        body=d.paragraphs[i+1] if False else None
        para=copy.deepcopy(d.paragraphs[i+1]._p) if False else None
        break
for i,p in enumerate(d.paragraphs):
    if p.text.startswith('Step 1b'):
        src=d.paragraphs[i-1]  # the Step 1 body paragraph
        b=copy.deepcopy(src._p); p._p.addnext(b)
        nb=docx.text.paragraph.Paragraph(b,p._parent)
        for r in nb.runs[1:]: r.text=''
        nb.runs[0].text=('We also log, from official national strategies only (no media), the growth sectors the government prioritises and what they need from nature, '
            'as the source states it. For the UK the Environment Agency names housing growth, net zero, food security and data centres, stating “All of these require water”, and projects a '
            '5 billion litre a day shortfall by 2055 without action; the AI Opportunities Action Plan adds AI Growth Zones for data centres that need land and power. '
            'These feed the page 2 context and the capital-deployment view.')
        break
d.save(sys.argv[2]); print('saved')
