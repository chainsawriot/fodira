db = connect( 'mongodb://localhost/main' );
db.articles.updateMany({"pubdate": ""}, { $unset: {"pubdate": "" }} );
db.articles.updateMany({}, [{"$set": {"pubdate": { "$toDate": "$pubdate"}}}]);
db.articles.createIndex({"pubdate": 1})
db.articles.createIndex({"pub": 1})
db.articles.createIndex({"link": 1}, {"unique": true})
