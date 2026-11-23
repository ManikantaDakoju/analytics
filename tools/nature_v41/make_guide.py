"""Generate 04_Slide_Writing_Guide_and_Example.md from a filled workbook (keys, limits, LBG example values, claims)."""
import openpyxl, sys
WB, OUT = sys.argv[1], sys.argv[2]
wb=openpyxl.load_workbook(WB,data_only=True); sc=wb['Slide_Content']
keys=[(sc.cell(r,1).value,sc.cell(r,4).value,sc.cell(r,5).value,sc.cell(r,8).value) for r in range(5,120) if sc.cell(r,1).value]
paste={sc.cell(r,10).value:(sc.cell(r,11).value or '',sc.cell(r,12).value or '') for r in range(5,121) if sc.cell(r,10).value}
claims=[[c for c in r[:8]] for r in wb['Claims_Register'].iter_rows(min_row=5,values_only=True) if r[0]]
STRUCT=[k for k,_,_,h in keys if h=='n/a (structural)']
RULES=[
'**Copy the style, never the facts.** The worked example is Lloyds Banking Group (UK). Never reuse its names, figures or wording for another client.',
'**Trace everything.** Every value except structural keys lists Claim_IDs, and every Claim_ID has a Claims_Register row whose primary trace resolves (EV-, NB-, CALC-, SC-, DEF-, ABB-, SRC-). No trace, no text.',
'**Numbers come from the workbook.** Percentages, totals, theme values and scores must equal a CALC- or SC- value. Exposure figures are quoted (EV-) and converted only for units (£365,599m → £365.6bn), with the conversion written in the claim derivation.',
'**Quotation marks mean exact words.** Only put words in quotation marks if the source uses exactly those words. Otherwise paraphrase without quotation marks and put the exact quote in the claim derivation.',
'**Action titles state the finding**, in 68 characters or fewer. Write "LBG meets that money mainly through flood risk in its mortgage book", not "Exposure analysis".',
'**Write words in full.** Spell out wherever space allows: biodiversity net gain (not BNG), key performance indicators (not KPIs), the full programme name (not HARP), "such as" (not e.g.), "about" (not c.), "including" (not incl.), "hectares" (not ha), months in full, "page" (not p.). Keep an abbreviation only for names or where the limit requires it (client short name, EY, NBSAP, ENCORE, country label, regulator codes such as PRA SS5/25).',
'**Every abbreviation you keep needs an ABB- glossary entry with key text** (e.g. "UK = United Kingdom"), whose expansion comes from a source. Excel lists it in the abbreviation key at the foot of the page. If no source spells it out (e.g. "ELMs" in the LBG framework), reword instead of abbreviating.',
'**Forward view from government sources.** Page 2 tiles about where the country is heading should cite national strategies (Prompt 3b), not the client’s own commentary, wherever possible.',
'**Facts, gaps and views are separate.** Page 4 "today" is facts only. "Gap" (shown on the slide as "Questions to explore") is an absence check that lists the sources searched (never "LBG fails to…"). "Opportunity" is an EY view with a rationale.',
'**Say "not yet visible"**, not "missing" or "lacking". The footnote says which documents were reviewed and which were not (e.g. the annual report).',
'**Write money and units consistently:** £12.1bn, £120m, 9.3%, about £41bn (rounded), 238,494 hectares. Shares of the client’s book take one decimal place everywhere (56.7%, not 57%). Use the country\'s currency symbol.',
'**Use UK English and a neutral tone.** Never name individuals, never use EY confidential data, never mention fees.',
'**Page 4 exposure panel:** s3_exp_stat is one figure that equals a CALC value (e.g. the nature-priority share of the book); s3_exp_text says what it is and how the largest sector is classified, using the client’s own definitions.',
'**Never draft a Why EY or EY offer page.** Page 2 (what the agent does) is fixed text in the skeleton.',
'**Check opportunities against the client’s own framework.** If the client already offers it, reword it as scale, connect or report, so the client cannot say "we already do that".',
'**Return only the two tables** (Slide_Content; Claims_Register), with no blank keys.']
WEAK={'s1_title':('UK nature context','A topic label, not a finding.'),'s1_ctx_why':('Nature is important for banks','No evidence, no figure, no source.'),
's1_callout_text':('is a lot of money','No subject; no traced value.'),'s1_tile1_stat':('Huge','Not a figure.'),'s1_tile1_text':('infrastructure spending is going up','No scope, no source figure.'),
's1_tile3_text':('water is running out','Alarmist and unsourced; give the figure, date and publisher.'),
's1_tile4_text':('Growth needs water (EY view)','Use the government’s own statement where one exists; name the sectors.'),
's1_rules':('Regulation is getting stricter','No rule named, no date.'),'s1_sowhat':('Banks should look at nature','Not specific to the evidence.'),
's2_title':('Client exposure','A label, not the finding.'),'s2_link_DM':('Lots of mortgages exposed to floods (est. £100bn)','Estimated; never estimate. Quote and calculate.'),
's2_link_W':('Utilities ~£12bn','Approximate; no % of book; informal sector name.'),'s2_sowhat':('The table shows the themes and exposures.','Describes the slide instead of the insight.'),
's3_col1_today':('LBG is doing lots on flooding','No traced facts.'),'s3_col1_gap':('LBG has failed to disclose flood risk','Critical tone; absence must be "not yet visible" with sources searched.'),
's3_col1_opp':('Big opportunity','Not specific; no rationale.'),'s3_exp_text':('of the book is green','Not the client’s own definition; vague; mixes up nature-priority and green finance.'),'s3_title':('LBG today','A label; no finding, no traced figure.'),'s3_sowhat':('EY can help with many things','Not the client\'s next step; page 5 covers EY.')}
L=['# Slide Writing Guide and Worked Example (v5.1)','',
'Knowledge file for the Copilot agent. It shows how to write every Slide_Content value for pages 3–5, and the Claims_Register rows behind them. Use it with SRC-METH (02_Methodology_and_Specifications). There is no Why EY page.','',
'## 1. Rules','']+[f'{i}. {r}' for i,r in enumerate(RULES,1)]+['','## 2. Field guide (limit = maximum characters)','','| Key | What to write | Limit | Good (LBG example) | Weak | Why weak |','|---|---|---|---|---|---|']
for k,g,lim,_ in keys:
    if k in WEAK: L.append(f'| {k} | {g} | {lim} | {paste[k][0]} | {WEAK[k][0]} | {WEAK[k][1]} |')
L+=['','Structural keys (no Claim_IDs needed): '+', '.join(STRUCT)+'.','','Excel writes these texts itself (do not write them): s1_ctx_what, s1_ctx_measured, s1_left_head, s1_left_sub, s1_callout_stat, the page 3 rows, h1–h3, the charts and the abbreviation keys (s1_key, s2_key, s3_key).','',
'## 3. Worked example: Lloyds Banking Group (UK), Slide_Content','','| Key | Value | Claim_IDs | Limit | Length |','|---|---|---|---|---|']
for k,g,lim,_ in keys:
    v,c=paste.get(k,('',''))
    assert len(v)<=lim,(k,len(v),lim)
    L.append(f'| {k} | {v} | {c} | {lim} | {len(v)} |')
L+=['','## 4. Worked example: Claims_Register (all rows)','','| Claim_ID | Slide | Key | Claim | Type | Primary trace | Other traces | Derivation / how checked |','|---|---|---|---|---|---|---|---|']
for c in claims: L.append('| '+' | '.join(('' if x is None else str(x)).replace('|','/') for x in c)+' |')
L+=['','## 5. Self-check before replying','',
'1. Every key in SRC-METH section 10 is present and within its limit.','2. Every non-structural value has Claim_IDs, and every Claim_ID appears in my Claims_Register table.',
'3. Every Calculated claim shows exactly the CALC value (same rounding); every Quoted fact matches its EV quote, with any unit change written in the derivation.',
'4. Quotation marks only around exact source words.','5. Words are written in full wherever space allows; every abbreviation kept has an ABB- entry with a sourced expansion and key text; no e.g., c., incl., ha, vs or shortened months.',
'6. Every EY view has a rationale and cites evidence; every absence check lists the sources searched.','7. Action titles state findings in 68 characters or fewer; neutral tone; no names, fees or EY confidential data.',
'8. No text was copied from the LBG example except structure and style.']
open(OUT,'w').write('\n'.join(L)+'\n'); print('guide',len(keys),'keys',len(claims),'claims')
