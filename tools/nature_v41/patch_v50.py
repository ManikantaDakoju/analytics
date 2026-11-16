"""v5.0: abbreviation key per page (Glossary H:K -> {{s1_key}} {{s2_key}} {{s3_key}}), page 4 portfolio-exposure panel
(Client_Exposure K:M -> Deck_Chart chart D, s3_exp_*), draft cover line, tighter page 4 limits, QA checks 24-26."""
import openpyxl, copy, sys, re
from openpyxl.styles import Font, Alignment
from openpyxl.worksheet.formula import ArrayFormula
F = 'Arial'
BL = Font(name=F, size=10, color='0000FF'); GN = Font(name=F, size=10, color='008000'); BD = Font(name=F, size=10, bold=True)
NT = Font(name=F, size=9, italic=True, color='595959'); BK = Font(name=F, size=10)

def hdr(ws, src, c):
    c.font = copy.copy(src.font); c.fill = copy.copy(src.fill); c.alignment = Alignment(wrap_text=True, vertical='center'); c.border = copy.copy(src.border)

def page_of(tok):
    k = tok.strip('{}')
    if k.endswith('_notes'): return 'notes'
    if k.startswith('cover_'): return 1
    if k in ('client_name', 'client_short'): return 0
    if k.startswith('s1_'): return 2
    if k.startswith('s2_') or re.match(r'r\d_', k) or k in ('country_label', 'currency_symbol'): return 3
    if k.startswith('s3_') or re.match(r'h\d$', k): return 4
    return 'other'

def clean(ref):
    x = ref
    for a, b in ((',', ' '), ('.', ' '), (';', ' '), (':', ' '), ('(', '( '), (')', ' )'), ('’', ' ’'), ("'", " '"), ('“', ' '), ('”', ' '), ('""', ' '), ('CHAR(10)', ' ')):
        a2 = a if a == 'CHAR(10)' else '"' + a + '"' if a != '""' else '""""'
        x = f'SUBSTITUTE({x},{a2},"{b}")'
    return '" "&' + x + '&" "'

DIG = '{"0","1","2","3","4","5","6","7","8","9"}'

def patch50(wb):
    if wb['Glossary']['H4'].value: return
    H = wb['Scoring']['A4']
    # ---------- Slide_Content: limits, guidance, new keys ----------
    s = wb['Slide_Content']; last = max(r for r in range(5, 200) if s.cell(r, 1).value)
    rows = {s.cell(r, 1).value: r for r in range(5, last + 1)}
    upd = {'cover_date': (50, 'Status and meeting date, e.g. Draft for internal review – 19 November 2026'),
           's2_section': (30, 'Section label, e.g. UK nature finance and LBG'),
           's3_title': (68, 'Action title: what the client’s book and position show, ≤68 characters'),
           's3_section': (40, 'Section label, e.g. LBG portfolio exposure and position')}
    for n in (1, 2, 3):
        upd[f's3_col{n}_today'] = (150, f'Theme ranked {n}: what the client does today (facts only), ≤150 characters')
        upd[f's3_col{n}_gap'] = (120, f'Theme ranked {n}: not yet visible in the documents reviewed (absence check), ≤120 characters')
        upd[f's3_col{n}_opp'] = (110, f'Theme ranked {n}: opportunity (EY view), ≤110 characters')
    for k, (lim, g) in upd.items():
        r = rows[k]; s.cell(r, 5).value = lim; s.cell(r, 4).value = g
    tr = next(r for r in range(5, last + 1) if isinstance(s.cell(r, 8).value, str) and s.cell(r, 8).value.startswith('='))
    new = [('s3_exp_stat', 'Exposure panel headline figure, e.g. the nature-priority share of the book (must equal a CALC value)', 7),
           ('s3_exp_sub', 'Exposure chart subtitle: what the bars show, the total exposure (£) and the date', 95),
           ('s3_exp_text', 'Words after the headline figure: what it is, and how the largest sector is classified (facts only)', 150)]
    for k, g, lim in new:
        last += 1; r = last; rows[k] = r
        s.cell(r, 1).value = k; s.cell(r, 1).font = BD; s.cell(r, 4).value = g; s.cell(r, 4).font = NT; s.cell(r, 5).value = lim
        for col, src in (('B', 'K'), ('C', 'L')):
            look = f'INDEX(${src}$5:${src}$120,MATCH($A{r},$J$5:$J$120,0))'
            s[f'{col}{r}'] = f'=IFERROR(IF({look}="","",{look}),"")'; s[f'{col}{r}'].font = GN
        s.cell(r, 6).value = f'=LEN(B{r})'; s.cell(r, 7).value = f'=IF(B{r}="","Missing",IF(F{r}>E{r},"Too long: "&(F{r}-E{r})&" over","OK"))'
        s.cell(r, 8).value = s.cell(tr, 8).value.replace(f'C{tr}', f'C{r}').replace(f'B{tr}', f'B{r}')
    # ---------- Deck_Tokens: page column, clean text, new tokens, keys ----------
    d = wb['Deck_Tokens']
    d['C4'] = 'Page (helper)'; d['D4'] = 'Text scanned for the abbreviation key (helper)'
    for c in ('C4', 'D4'): hdr(d, d['A4'], d[c])
    r = max(x for x in range(5, 300) if d.cell(x, 1).value) + 1
    for k, _, _ in new:
        d.cell(r, 1).value = '{{' + k + '}}'; d.cell(r, 1).font = BD
        d.cell(r, 2).value = f'=IF(Slide_Content!$B${rows[k]}="","",Slide_Content!$B${rows[k]})'; d.cell(r, 2).font = GN; r += 1
    # also strip the abbreviation "c." from the Excel-written context text
    for x in range(5, r):
        v = d.cell(x, 2).value
        if isinstance(v, str) and '(c."' in v: d.cell(x, 2).value = v.replace('(c."', '(about "')
    for x in range(5, r):
        p = page_of(d.cell(x, 1).value); d.cell(x, 3).value = p
        if isinstance(p, int) and p >= 0: d.cell(x, 4).value = '=' + clean(f'B{x}'); d.cell(x, 4).font = GN
    # fixed skeleton text that can hold abbreviations
    d['G3'] = 'Fixed skeleton text scanned for the abbreviation key (edit if you change the skeleton)'; d['G3'].font = NT
    for c, v in (('G4', 'Page'), ('H4', 'Fixed text on the slide'), ('I4', 'Scanned text (helper)')):
        d[c] = v; hdr(d, d['A4'], d[c])
    fixed = [(3, 'Growth sectors compared with book: government priority sectors · client exposure (% of book) · water dependency (ENCORE, 0–1)'),
             (4, 'Opportunity – EY view'), (4, 'Bars: share of total exposure; yellow = nature-priority part, as disclosed')]
    for i, (p, t) in enumerate(fixed, 5):
        d.cell(i, 7).value = p; d.cell(i, 8).value = t; d.cell(i, 9).value = '=' + clean(f'H{i}')
        d.cell(i, 7).font = BL; d.cell(i, 8).font = BL; d.cell(i, 9).font = GN
    d.column_dimensions['C'].width = 9; d.column_dimensions['D'].width = 30; d.column_dimensions['H'].width = 50; d.column_dimensions['I'].width = 30
    # key tokens
    for n, col in ((1, 'I'), (2, 'J'), (3, 'K')):
        d.cell(r, 1).value = '{{' + f's{n}_key' + '}}'; d.cell(r, 1).font = BD; d.cell(r, 3).value = 'key'
        ref = f'B{r}'
        f = (f'=IF(COUNTIF(Glossary!${col}$5:${col}$154,"Yes")=0,"","Abbreviations: "&_xlfn.TEXTJOIN("; ",TRUE,'
             f'IF(Glossary!${col}$5:${col}$154="Yes",IF(Glossary!$G$5:$G$154="",Glossary!$B$5:$B$154&" = [add key text]",Glossary!$G$5:$G$154),"")))')
        d[ref] = ArrayFormula(ref, f); d[ref].font = GN; r += 1
    d['A2'] = (d['A2'].value or '') + ' Column C/D and G:I are helpers for the abbreviation key; do not copy them.'
    # ---------- Glossary: key text and page detection ----------
    g = wb['Glossary']
    # key text goes in G (agent output pastes as one block A:G); the 'appears in slide text' formula moves to H
    g['H4'] = g['G4'].value; hdr(g, g['A4'], g['H4'])
    for x in range(5, 155):
        g[f'H{x}'] = g[f'G{x}'].value; g[f'H{x}'].font = GN; g[f'G{x}'] = None
    q0 = wb['QA_Log']
    for x in range(5, 60):
        v = q0.cell(x, 3).value
        if isinstance(v, str) and 'Glossary!G5:G154' in v: q0.cell(x, 3).value = v.replace('Glossary!G5:G154', 'Glossary!H5:H154')
    for c, v, w in (('G', 'Key text (shown in the abbreviation key at the foot of the page, e.g. "UK = United Kingdom")', 40),
                    ('I', 'On page 2?', 9), ('J', 'On page 3?', 9), ('K', 'On page 4?', 9)):
        g[f'{c}4'] = v; hdr(g, g['A4'], g[f'{c}4']); g.column_dimensions[c].width = w
    g['H3'] = 'Abbreviation key: every Abbreviation or Notation entry found on a page is listed at the foot of that page (Deck_Tokens s1_key–s3_key). Notation = a unit written straight after a number (bn, m).'; g['H3'].font = NT
    DT = 'Deck_Tokens!$C$5:$C$200'; DD = 'Deck_Tokens!$D$5:$D$200'; FG = 'Deck_Tokens!$G$5:$G$20'; FI = 'Deck_Tokens!$I$5:$I$20'
    for x in range(5, 155):
        for c, p in (('I', 2), ('J', 3), ('K', 4)):
            on = f'(({DT}={p})+({DT}=0))'
            ab = f'SUMPRODUCT({on}*ISNUMBER(FIND(" "&$B{x}&" ",{DD})))+SUMPRODUCT(({FG}={p})*ISNUMBER(FIND(" "&$B{x}&" ",{FI})))'
            nt = f'SUMPRODUCT({on}*ISNUMBER(FIND({DIG}&$B{x},{DD})))'
            g[f'{c}{x}'] = f'=IF($B{x}="","",IF($C{x}="Abbreviation",IF({ab}>0,"Yes","No"),IF($C{x}="Notation",IF({nt}>0,"Yes","No"),"")))'
            g[f'{c}{x}'].font = GN
        g[f'G{x}'].font = BL
    keytext = {'ABB-NBSAP': 'NBSAP = National Biodiversity Strategy and Action Plan', 'ABB-EY': 'EY = Ernst & Young',
               'ABB-ENCORE': 'ENCORE = Exploring Natural Capital Opportunities, Risks and Exposure'}
    for x in range(5, 155):
        if g.cell(x, 1).value in keytext: g.cell(x, 7).value = keytext[g.cell(x, 1).value]
    gx = next(x for x in range(34, 60) if not g.cell(x, 1).value)
    for row in (['ABB-BN', 'bn', 'Notation', 'billion (after an amount, e.g. £60bn)', 'SRC-METH', 'Section 9 (style rules)', 'bn = billion'],
                ['ABB-M', 'm', 'Notation', 'million (after an amount, e.g. £120m)', 'SRC-METH', 'Section 9 (style rules)', 'm = million']):
        for c, v in enumerate(row, 1):
            if v is not None: g.cell(gx, c).value = v; g.cell(gx, c).font = BL
        gx += 1
    # ---------- Client_Exposure: optional disclosed shares + chart rank ----------
    ce = wb['Client_Exposure']
    for c, v, w in (('K', 'Nature priority % of Group (as disclosed; optional)', 14), ('L', 'Increased climate risk % of Group (as disclosed; optional)', 14), ('M', 'Chart D rank (formula)', 10)):
        ce[f'{c}4'] = v; hdr(ce, ce['A4'], ce[f'{c}4']); ce.column_dimensions[c].width = w
    for x in range(5, 201):
        ce[f'M{x}'] = f'=IF(H{x}="Sector total",COUNTIFS($H$5:$H$200,"Sector total",$E$5:$E$200,">"&E{x})+1,"")'; ce[f'M{x}'].font = GN
        for c in ('K', 'L'): ce[f'{c}{x}'].number_format = '0.0%'; ce[f'{c}{x}'].font = BL
    ce['K3'] = 'K and L: copy the disclosed % of Group for each sector total, if the client reports them (as fractions, e.g. 0.011). They feed chart D on page 4.'; ce['K3'].font = NT
    # ---------- Deck_Chart: chart D ----------
    dc = wb['Deck_Chart']
    dc['A2'] = (dc['A2'].value or '') + ' Chart D (page 4, client book by sector): A29:C41 (delete empty rows after pasting).'
    dc['A28'] = 'Chart D – page 4: client book by sector, sorted by exposure (% of Group total)'; dc['A28'].font = BD
    for c, v in (('A29', 'Category'), ('B29', 'Nature-priority part'), ('C29', 'Rest of sector')): dc[c] = v; dc[c].font = BD
    GT = 'SUMIFS(Client_Exposure!$E$5:$E$200,Client_Exposure!$H$5:$H$200,"Group total")'
    for n in range(1, 13):
        x = 29 + n; m = f'MATCH({n},Client_Exposure!$M$5:$M$200,0)'
        dc[f'A{x}'] = f'=IFERROR(INDEX(Client_Exposure!$C$5:$C$200,{m})&"  "&TEXT(INDEX(Client_Exposure!$E$5:$E$200,{m})/{GT},"0.0%"),"")'
        dc[f'B{x}'] = f'=IFERROR(N(INDEX(Client_Exposure!$K$5:$K$200,{m})),"")'
        dc[f"C{x}"] = f"=IFERROR(MAX(0,INDEX(Client_Exposure!$E$5:$E$200,{m})/{GT}-N(INDEX(Client_Exposure!$K$5:$K$200,{m}))),\"\")"
        for c in 'ABC': dc[f'{c}{x}'].font = GN
        dc[f'B{x}'].number_format = '0.0%'; dc[f'C{x}'].number_format = '0.0%'
    # ---------- QA ----------
    q = wb['QA_Log']; r = max(x for x in range(5, 100) if q.cell(x, 1).value) + 1
    checks = [('Abbreviation key: every abbreviation or notation on pages 2–4 has key text (Glossary G)',
               '=IF(SUMPRODUCT(((Glossary!C5:C154="Abbreviation")+(Glossary!C5:C154="Notation"))*(((Glossary!I5:I154="Yes")+(Glossary!J5:J154="Yes")+(Glossary!K5:K154="Yes"))>0)*(Glossary!G5:G154=""))=0,"Pass",SUMPRODUCT(((Glossary!C5:C154="Abbreviation")+(Glossary!C5:C154="Notation"))*(((Glossary!I5:I154="Yes")+(Glossary!J5:J154="Yes")+(Glossary!K5:K154="Yes"))>0)*(Glossary!G5:G154=""))&" abbreviations need key text")'),
              ('Abbreviation key fits the footer (≤ 330 characters on each page)',
               '=IF(MAX(LEN(INDEX(Deck_Tokens!B:B,MATCH("{{s1_key}}",Deck_Tokens!A:A,0))),LEN(INDEX(Deck_Tokens!B:B,MATCH("{{s2_key}}",Deck_Tokens!A:A,0))),LEN(INDEX(Deck_Tokens!B:B,MATCH("{{s3_key}}",Deck_Tokens!A:A,0))))<=330,"Pass","Too long: spell out more abbreviations in the slide text")'),
              ('Every mapping row has Mapping strength "Direct" or "Indirect" (catches swapped columns)',
               '=IF(SUMPRODUCT((NBSAP_Mapping!A5:A304<>"")*(NBSAP_Mapping!G5:G304<>"Direct")*(NBSAP_Mapping!G5:G304<>"Indirect"))=0,"Pass",SUMPRODUCT((NBSAP_Mapping!A5:A304<>"")*(NBSAP_Mapping!G5:G304<>"Direct")*(NBSAP_Mapping!G5:G304<>"Indirect"))&" rows: check columns G and H")'),
              ('LOGIC: sector totals in Client_Exposure add up to the Group total (chart D shows the whole book)',
               '=IF(COUNTIF(Client_Exposure!H5:H200,"Group total")<>1,"Fail – add exactly one Group total row",IF(ABS(SUMIFS(Client_Exposure!E5:E200,Client_Exposure!H5:H200,"Sector total")/SUMIFS(Client_Exposure!E5:E200,Client_Exposure!H5:H200,"Group total")-1)<=0.005,"Pass","Warning: sector totals = "&TEXT(SUMIFS(Client_Exposure!E5:E200,Client_Exposure!H5:H200,"Sector total")/SUMIFS(Client_Exposure!E5:E200,Client_Exposure!H5:H200,"Group total"),"0.0%")&" of Group total – add the missing sectors or say so on page 4"))')]
    for t, f in checks:
        q.cell(r, 1).value = r - 4; q.cell(r, 2).value = t; q.cell(r, 3).value = f; r += 1
    rd = wb['README']; rd.cell(1, 1).value = 'Nature Opportunity Master Template (v5.0)'; x = rd.max_row + 1
    rd.cell(x, 1).value = 'v5.0 changes'
    rd.cell(x, 2).value = ('Abbreviation key at the foot of pages 2–4, built from Glossary G and I:K (Deck_Tokens s1_key–s3_key). Page 4 portfolio-exposure panel: '
                           'Client_Exposure K:M feed chart D (Deck_Chart A30:C41); new keys s3_exp_stat, s3_exp_sub, s3_exp_text. Cover line carries the review status. '
                           'Page 4 cell limits 150/120/110. QA checks 24–27 (key text, key length, mapping strength, sector totals add up).')

if __name__ == '__main__':
    wb = openpyxl.load_workbook(sys.argv[1]); patch50(wb); wb.save(sys.argv[2]); print('v5.0', sys.argv[2])
