"""v4.4: treat the client's annual / sustainability report as an expected core source (QA check + caveat rule)."""
import openpyxl, sys
from openpyxl.styles import Font
B=Font(name='Arial',size=10)
def patch44(wb):
    q=wb['QA_Log']
    if any((q.cell(r,2).value or '').startswith('Core source') for r in range(5,60)): return
    r=max(x for x in range(5,100) if q.cell(x,1).value)+1
    q.cell(r,1).value=r-4; q.cell(r,2).value='Core source: client annual / sustainability report reviewed (Sources type "Annual / sustainability report", Included = Y)'
    q.cell(r,3).value=('=IF(COUNTIFS(Sources!$E$5:$E$60,"Annual / sustainability report",Sources!$I$5:$I$60,"Y")>0,"Pass",'
                       'IF(ISNUMBER(SEARCH("Annual",INDEX(Slide_Content!$B$5:$B$120,MATCH("s3_footnote",Slide_Content!$A$5:$A$120,0)))),'
                       '"Warning: not reviewed – page 4 caveat present","Fail: add the report or a page 4 caveat"))')
    for c in (1,2,3): q.cell(r,c).font=B
    so=wb['Sources']; so['A2']=(so['A2'].value or '')+' Core sources expected for every client: annual / sustainability report (Type exactly "Annual / sustainability report"); if not available, keep a row with Included = N and the reason.'
    rd=wb['README']; x=rd.max_row+1; rd.cell(x,1).value='v4.4 changes'; rd.cell(x,2).value='Annual / sustainability report is an expected core source: QA check (Pass / Warning with page 4 caveat / Fail).'
    rd.cell(1,1).value='Nature Opportunity Master Template (v4.4)'
if __name__=='__main__':
    wb=openpyxl.load_workbook(sys.argv[1]); patch44(wb)
    if len(sys.argv)>3:  # LBG: record the missing report with its reason
        so=wb['Sources']
        for r in range(5,60):
            if so.cell(r,1).value=='SRC-06':
                so.cell(r,10).value='Expected core source. Uploads to the EY review session failed (file blocked before upload; cause on the client side not identified). Attach it in the Copilot run (Prompts 1–2); until then page 4 gaps are provisional (caveat in s3_footnote).'
    wb.save(sys.argv[2]); print('v4.4',sys.argv[2])
