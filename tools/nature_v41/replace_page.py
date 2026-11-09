"""Replace the content of one skeleton page with a freshly built page (shapes + charts), keeping the EY template title, client line and notes."""
import re, zipfile, sys, shutil, os
SKEL, SLIDE_NO, NEWPAGE, OUT = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
zin=zipfile.ZipFile(SKEL); new=zipfile.ZipFile(NEWPAGE)
sp=f'ppt/slides/slide{SLIDE_NO}.xml'; rp=f'ppt/slides/_rels/slide{SLIDE_NO}.xml.rels'
x=zin.read(sp).decode(); rels=zin.read(rp).decode()
# drop old content shapes (ids >= 1001 were added by the build) and the old chart relationships
i=x.index('<p:cNvPr id="1001"'); pre=x[:i]
st=max(pre.rfind(t) for t in ('<p:sp>','<p:graphicFrame>','<p:cxnSp>','<p:pic>'))
x=x[:st]+x[x.rindex('</p:spTree>'):]
old=re.findall(r'<Relationship Id="(rIdCh\d+)"[^>]*Target="\.\./charts/(chart\d+)\.xml"/>',rels)
rels=re.sub(r'<Relationship Id="rIdCh\d+"[^>]*/>','',rels)
drop={f'ppt/charts/{c}.xml' for _,c in old}|{f'ppt/charts/_rels/{c}.xml.rels' for _,c in old}|{f'ppt/embeddings/{c}_data.xlsx' for _,c in old}
body=new.read('ppt/slides/slide1.xml').decode(); body=body[body.index('</p:grpSpPr>')+12:body.rindex('</p:spTree>')]
k=[1000]
def renum(m): k[0]+=1; return f'<p:cNvPr id="{k[0]}"'
body=re.sub(r'<p:cNvPr id="\d+"',renum,body)
nrels=new.read('ppt/slides/_rels/slide1.xml.rels').decode()
ct=zin.read('[Content_Types].xml').decode()
for _,c in old: ct=ct.replace(f'<Override PartName="/ppt/charts/{c}.xml" ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/>','')
add={}
names=[n for _,n in old] or [f'chart{300+SLIDE_NO}']
for idx,(rid,tgt) in enumerate(re.findall(r'Id="(rId\d+)" Type="[^"]*/chart" Target="/ppt/charts/(chart\d+)\.xml"',nrels)):
    name=names[idx] if idx<len(names) else f'chart{300+10*SLIDE_NO+idx}'
    crel=new.read(f'ppt/charts/_rels/{tgt}.xml.rels').decode(); emb=re.search(r'embeddings/([^"]+)',crel).group(1)
    add[f'ppt/charts/{name}.xml']=new.read(f'ppt/charts/{tgt}.xml')
    add[f'ppt/charts/_rels/{name}.xml.rels']=crel.replace(emb,f'{name}_data.xlsx').encode()
    add[f'ppt/embeddings/{name}_data.xlsx']=new.read(f'ppt/embeddings/{emb}')
    nid=f'rIdCh{name[5:]}'; body=body.replace(f'r:id="{rid}"',f'r:id="{nid}"')
    rels=rels.replace('</Relationships>',f'<Relationship Id="{nid}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart" Target="../charts/{name}.xml"/></Relationships>')
    ct=ct.replace('</Types>',f'<Override PartName="/ppt/charts/{name}.xml" ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/></Types>')
x=x[:x.rindex('</p:spTree>')]+body+x[x.rindex('</p:spTree>'):]
zout=zipfile.ZipFile(OUT,'w',zipfile.ZIP_DEFLATED)
for it in zin.infolist():
    n=it.filename
    if n in drop or n in add: continue
    data={sp:x.encode(),rp:rels.encode(),'[Content_Types].xml':ct.encode()}.get(n) or zin.read(n)
    zout.writestr(it,data)
for n,d in add.items(): zout.writestr(n,d)
zout.close(); print('replaced page',SLIDE_NO,'->',OUT)
