"""v5.1 (after the 19 November review with Helen): UK priority tick (theme takes >=10% of identified commitments),
chart D labels with amount and share, top-three share calculation, 'beyond the top three' strip (s2_other1/2) replacing
the growth-sector strip, slide 3 lead band (s2_lead), slide 4 note (s3_exp_note), wider limits for the reviewed wording,
context band and footnote formulas in the reviewed phrasing."""
import openpyxl, sys
from openpyxl.styles import Font
F = 'Arial'
GN = Font(name=F, size=10, color='008000'); BD = Font(name=F, size=10, bold=True); NT = Font(name=F, size=9, italic=True, color='595959'); BL = Font(name=F, size=10, color='0000FF')

def patch51(wb):
    tv = wb['Theme_View']
    if tv['X4'].value: return
    # ---- UK priority tick
    tv['X4'] = 'UK priority? (✓ = theme takes at least the threshold share of identified commitments)'; tv['X4'].font = BD
    tv['W2'] = 'UK priority threshold'; tv['X2'] = 0.1; tv['X2'].number_format = '0%'; tv['X2'].font = BL
    for r in range(5, 10):
        tv[f'X{r}'] = f'=IF(N{r}="","",IF(N{r}>=$X$2,"✓",""))'; tv[f'X{r}'].font = GN
    tv.column_dimensions['X'].width = 18
    # ---- Calculations: top-three share (by score)
    c = wb['Calculations']; r = next(x for x in range(25, 29) if not c.cell(x, 1).value)
    for j, v in enumerate(['CALC-TOP3-S', 'Top three themes by score: combined share of country total', '=SUMPRODUCT((Theme_View!K5:K9<=3)*Theme_View!N5:N9)', '%',
                           'Sum of shares of the three highest-scoring themes', 'CALC-S-*', 'SRC-EY'], 1):
        c.cell(r, j).value = v
    c.cell(r, 3).number_format = '0.0%'
    # ---- Glossary
    g = wb['Glossary']; gx = next(x for x in range(34, 60) if not g.cell(x, 1).value)
    for j, v in enumerate(['DEF-UKPRIO', 'UK priority (✓)', 'Definition', 'Theme that takes 10% or more of the identified national NBSAP commitments in the EY sizing workbook (threshold in Theme_View X2)', 'SRC-METH', 'Section 10'], 1):
        g.cell(gx, j).value = v
    # ---- Slide_Content: replace growth keys with the 'beyond the top three' strip; add lead band and note; widen limits
    s = wb['Slide_Content']; rows = {s.cell(x, 1).value: x for x in range(5, 200) if s.cell(x, 1).value}
    ren = {'s2_grow1_head': ('s2_other1_head', 'Theme ranked 4 (beyond the top three): theme name', 40),
           's2_grow1_text': ('s2_other1_text', 'Theme ranked 4: the client link and an opportunity (EY view), with figures from Calculations', 150),
           's2_grow2_head': ('s2_other2_head', 'Theme ranked 5: theme name', 40),
           's2_grow2_text': ('s2_other2_text', 'Theme ranked 5: the client link and an opportunity (EY view), with figures from Calculations', 150)}
    for old, (new, gd, lim) in ren.items():
        x = rows[old]; s.cell(x, 1).value = new; s.cell(x, 4).value = gd; s.cell(x, 5).value = lim
    spare = [rows[k] for k in ('s2_grow3_head', 's2_grow3_text', 's2_grow4_head', 's2_grow4_text')]
    for x, (k, gd, lim) in zip(spare, [('s2_lead', 'One-line lead band under the page 3 title: the national headline and what the table does', 160),
                                       ('s3_exp_note', 'Note under the exposure panel: how the client classifies its largest sector (client’s own terms)', 130)]):
        s.cell(x, 1).value = k; s.cell(x, 4).value = gd; s.cell(x, 5).value = lim
    for x in spare[2:]:
        for col in range(1, 9): s.cell(x, col).value = None
    LIM = {'cover_date': 50, 's1_title': 68, 's1_callout_text': 75, 's1_sub_r': 80, 's1_tile1_stat': 16, 's1_tile2_stat': 16, 's1_tile3_stat': 16, 's1_tile4_stat': 16,
           's1_rules_lead': 50, 's1_rules': 140, 's1_sowhat': 200, 's2_section': 60, 's2_sowhat': 220, 's3_section': 60, 's3_sowhat': 220,
           's3_exp_stat': 16, 's3_exp_text': 90, 's1_ctx_why': 150}
    for t in ('W', 'AFF', 'UD', 'DM', 'P'): LIM[f's2_link_{t}'] = 160
    for n in (1, 2, 3): LIM[f's3_col{n}_today'] = 240; LIM[f's3_col{n}_gap'] = 170; LIM[f's3_col{n}_opp'] = 170
    for k, v in LIM.items(): s.cell(rows[k], 5).value = v
    s.cell(rows['cover_date'], 4).value = 'Status and meeting date, e.g. For discussion – 10 December 2026'
    s.cell(rows['s2_title'], 4).value = 'Action title: the priority themes and their share of national commitments, ≤68 characters'
    s.cell(rows['s3_title'], 4).value = 'Action title: the nature-priority share of the client’s book (not its largest sector), ≤68 characters'
    # ---- Deck_Tokens
    d = wb['Deck_Tokens']; trow = {d.cell(x, 1).value: x for x in range(5, 300) if d.cell(x, 1).value}
    for old, (new, _, _) in ren.items():
        x = trow['{{' + old + '}}']; d.cell(x, 1).value = '{{' + new + '}}'
    for old, new, sc in (('s2_grow3_head', 's2_lead', spare[0]), ('s2_grow3_text', 's3_exp_note', spare[1])):
        x = trow['{{' + old + '}}']; d.cell(x, 1).value = '{{' + new + '}}'
        d.cell(x, 2).value = f'=IF(Slide_Content!$B${sc}="","",Slide_Content!$B${sc})'; d.cell(x, 3).value = 3 if new == 's2_lead' else 4
    for old in ('s2_grow4_head', 's2_grow4_text'):
        x = trow['{{' + old + '}}']
        for col in range(1, 5): d.cell(x, col).value = None
    # UK priority tick tokens (in score order), placed in the freed rows and after
    free = [trow['{{s2_grow4_head}}'], trow['{{s2_grow4_text}}']]
    nxt = max(x for x in range(5, 300) if d.cell(x, 1).value) + 1
    slots = free + [nxt, nxt + 1, nxt + 2]
    for n, x in zip(range(1, 6), slots):
        d.cell(x, 1).value = '{{' + f'r{n}_uk' + '}}'; d.cell(x, 1).font = BD
        d.cell(x, 2).value = f'=INDEX(Theme_View!$X$5:$X$9,MATCH({n},Theme_View!$K$5:$K$9,0))&""'; d.cell(x, 2).font = GN; d.cell(x, 3).value = 'other'
    lab, cur, cli = 'Inputs!$B$14', 'Inputs!$B$16', 'Inputs!$B$13'
    # reviewed formats: '£23.0bn | 38%' and 'Water | 4.5'; country name token for the abbreviation key on the agent slide
    for n in range(1, 6):
        x = trow['{{' + f'r{n}_money' + '}}']; d.cell(x, 2).value = d.cell(x, 2).value.replace('"bn · "', '"bn | "')
    for n in (1, 2, 3):
        x = trow['{{' + f'h{n}' + '}}']; d.cell(x, 2).value = d.cell(x, 2).value.replace('&"   "&', '&" | "&')
    x = max(y for y in range(5, 300) if d.cell(y, 1).value) + 1
    d.cell(x, 1).value = '{{country_name}}'; d.cell(x, 1).font = BD; d.cell(x, 2).value = '=Inputs!$B$7'; d.cell(x, 2).font = GN; d.cell(x, 3).value = 'other'
    d.cell(trow['{{s1_ctx_what}}'], 2).value = (f'=IF(OR(Inputs!$B$8="",Inputs!$B$8="N/A"),"The "&{lab}&" has not yet published a National Biodiversity Strategy and Action Plan (NBSAP).",'
                                                 f'"The "&{lab}&" sets out its nature priorities in the "&Inputs!$B$8&", its National Biodiversity Strategy and Action Plan (NBSAP).")')
    d.cell(trow['{{s1_ctx_measured}}'], 2).value = (f'="We mapped "&TEXT(Calculations!$C$5,"0")&" "&{lab}&" commitments with stated funding amounts (about "&{cur}&TEXT(Calculations!$C$6,"0")&"bn, indicative) across five nature-related themes."')
    d.cell(trow['{{s1_left_head}}'], 2).value = f'={lab}&" nature investment by theme"'
    d.cell(trow['{{s1_left_sub}}'], 2).value = f'={lab}&" NBSAP commitments by theme ("&{cur}&"bn; % of total; indicative)"'
    d.cell(trow['{{s2_footnote}}'], 2).value = (f'="¹ Composite score from public data: materiality to "&{cli}&" "&TEXT(Rubric!$C$5,"0%")&", action gap "&TEXT(Rubric!$C$6,"0%")&", "&{lab}&" NBSAP emphasis "&TEXT(Rubric!$C$7,"0%")'
                                                f'&", regulatory and market momentum "&TEXT(Rubric!$C$8,"0%")&", market opportunity "&TEXT(Rubric!$C$9,"0%")&". ² ✓ = theme takes "&TEXT(Theme_View!$X$2,"0%")&" or more of identified "&{lab}&" commitments. Links to "&{cli}&" sectors are EY view."')
    # fixed skeleton text scanned for the key
    d['H5'] = 'UK priority²'; d['G6'] = 4; d['H6'] = 'Evidence today · Evidence identified in public disclosures · Areas to explore · Not identified in the public documents reviewed · Potential opportunity · EY perspective'
    d['H7'] = 'Bar length = share of total exposure | Yellow = part of each sector classed by the client as a nature priority'
    # ---- Deck_Chart: chart D labels with amount and share
    dc = wb['Deck_Chart']; GT = 'SUMIFS(Client_Exposure!$E$5:$E$200,Client_Exposure!$H$5:$H$200,"Group total")'
    for n in range(1, 13):
        x = 29 + n; m = f'MATCH({n},Client_Exposure!$M$5:$M$200,0)'
        dc[f'A{x}'] = (f'=IFERROR(INDEX(Client_Exposure!$C$5:$C$200,{m})&"  "&{cur}&TEXT(INDEX(Client_Exposure!$E$5:$E$200,{m})/1000,"0.0")&"bn | "'
                       f'&TEXT(INDEX(Client_Exposure!$E$5:$E$200,{m})/{GT},"0.0%"),"")')
    q = wb['QA_Log']
    for x in range(5, 60):
        v = q.cell(x, 3).value
        if isinstance(v, str) and 'page 4 caveat' in v: q.cell(x, 3).value = v.replace('page 4 caveat', 'portfolio-page caveat')
    rd = wb['README']; rd.cell(1, 1).value = 'Nature Opportunity Master Template (v5.1)'; x = rd.max_row + 1
    rd.cell(x, 1).value = 'v5.1 changes'
    rd.cell(x, 2).value = ('After the 19 November review: UK priority tick (Theme_View X, threshold X2; tokens r1_uk–r5_uk); CALC-TOP3-S; chart D labels show amount and share; '
                           'growth-sector strip replaced by s2_other1/2 (themes ranked 4 and 5); new keys s2_lead and s3_exp_note; wider limits; context band and footnote reworded.')

if __name__ == '__main__':
    wb = openpyxl.load_workbook(sys.argv[1]); patch51(wb); wb.save(sys.argv[2]); print('v5.1', sys.argv[2])
