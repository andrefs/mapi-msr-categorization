const mongo = require('promised-mongo');
const db = mongo('metacache', ['contents']);
const fs = require('fs-extra');
const htmlToText = require('html-to-text');
const Promise = require('bluebird');

async function dumpDocs(filter, folder, fields){
  console.log('['+(new Date().toISOString())+'] Dumping',filter,'into '+folder);
  const _filter = {
    ...filter,
    'Source.Name': 'Observador',
    Type: "sapo.obj.creativework.article",
    Body: {$exists: true},
    $where: 'this.Body.length > 100'
  };

  const docs = await db.contents.find(_filter).toArray();
  return Promise.mapSeries(docs,
    doc => {
      const _fields = fields || ['Pretitle','Title','Subtitle', 'Lead'];

      let txt = _fields.map(f => doc[f]);
      if(doc.Body)    {
        const text = htmlToText.fromString(doc.Body);
        txt.push(text);
      }
      return fs.writeFile(folder+'/'+doc._id, txt.join('\n'));
    });
}

dumpDocs({'CategoryPaths':'Mundo'}, './_/mundo')
  .then(() => dumpDocs({'CategoryPaths':'País'},     './_/pais'))
  .then(() => dumpDocs({'CategoryPaths':'Economia'}, './_/economia'))
  .then(() => dumpDocs({'CategoryPaths':'Política'}, './_/politica'))
  .then(() => dumpDocs({'CategoryPaths':'Desporto'}, './_/desporto'))
  .then(() => dumpDocs({'CategoryPaths':'Ciência'},  './_/ciencia'))
  .then(() => dumpDocs({'CategoryPaths':'Cultura\n\n\tCinema\n\tMúsica\n\tLivros'}, './_/cultura'))
  .then(() => dumpDocs({'CategoryPaths':'Música'},   './_/musica'))
  .then(() => {
    console.log('Finished');
    process.exit(0);
  });
