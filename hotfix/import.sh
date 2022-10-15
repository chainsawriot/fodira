mongoimport --type csv -d main -c articles --headerline --drop ../rawdata/articles_clean.csv
mongosh --file fix.js
