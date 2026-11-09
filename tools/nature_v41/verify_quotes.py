"""Check that each evidence quote appears word for word on its PDF page."""
import subprocess, re
INP='/home/user/analytics/lbg-nature-poc/inputs/'
PDF={'SRC-01':INP+'working-with-nature-report.pdf','SRC-02':INP+'lbg-built-to-last-report.pdf','SRC-05':INP+'2025-lbg-sustainable-financing-framework.pdf',
     'SRC-07':INP+'United_Kingdom_ai_opportunities_action_plan.pdf','SRC-08':INP+'United_Kingdom_2025_EA_National_Framework_Water_Resources_summary.pdf'}
_c={}
def norm(s):
    for a,b in (('’',"'"),('‘',"'"),('“','"'),('”','"'),('–','-'),('—','-'),('ﬁ','fi'),('ﬂ','fl')): s=s.replace(a,b)
    return re.sub(r'\s+',' ',re.sub(r'-\s*\n\s*','-',s)).strip()
def page(src,p):
    if (src,p) not in _c:
        _c[(src,p)]=norm(subprocess.run(['pdftotext','-q','-f',str(p),'-l',str(p),PDF[src],'-'],capture_output=True,text=True).stdout)
    return _c[(src,p)]
def check(src,pdf_page,quote):
    """Return 'exact' if the quote is on pdf_page, else where it was found, else 'NOT FOUND'."""
    q=norm(quote)
    if q in page(src,pdf_page): return 'exact'
    for d in (-1,1,-2,2):
        if pdf_page+d>0 and q in page(src,pdf_page+d): return f'found on PDF p{pdf_page+d}'
    return 'NOT FOUND'
