require'pg'
pg_backup = PG.connect({dbname: 'sblog_backup', host: 'localhost',:user => "david"})
pg_new = PG.connect({dbname: 'sblog', host: 'localhost',:user => "david"})
insertQuery = "INSERT INTO entries (title,url,summary,body,markdown ,published,created_at,updated_at) \
  VALUES ($1, $2,$3,$4,$5,$6,$7, $8) RETURNING id"
pg_new.prepare('insert_entry', insertQuery)
pg_new.prepare('insert_tag','INSERT INTO tags ( name ) VALUES ( $1 ) RETURNING id')
pg_new.prepare('insert_tagging','INSERT INTO taggings (entry_id,tag_id) VALUES ($1,$2) RETURNING id')

acount = pg_backup.exec('SELECT count(id) FROM articles')[0]["count"]
bcount = pg_backup.exec('SELECT count(id) FROM bookmarks')[0]["count"]
tcount = pg_backup.exec("SELECT count(id) FROM tags")[0]["count"]
puts acount
puts bcount
puts tcount
i = 1
while i <= tcount.to_i do
  r = pg_backup.exec("SELECT * FROM tags WHERE id = #{i}")[0]
  pg_new.exec_prepared("insert_tag",[r["name"]])
  i = i + 1
end
i = 1
while i <= acount.to_i do
  a = pg_backup.exec("SELECT * FROM articles WHERE id = #{i}")[0]
  aid = a["id"]
  ts = pg_backup.exec("SELECT t.id FROM tags as t, taggings as tg WHERE \
   t.id = tg.tag_id AND tg.related_type = 2 AND tg.related_id = #{aid}")
  e = pg_new.exec_prepared("insert_entry",[
      a["title"],"",a["summary"],a["body"],a["markdown"],a["published"],a["created_at"],a["updated_at"]
    ])[0]
  eid = e["id"]
  ts.each do |t|
    tid = t["id"]
    pg_new.exec_prepared("insert_tagging",[eid,tid])
  end
  i = i + 1
end
i = 1
while i <= bcount.to_i do
  a = pg_backup.exec("SELECT * FROM bookmarks WHERE id = #{i}")[0]
  aid = a["id"]
  ts = pg_backup.exec("SELECT t.id FROM tags as t, taggings as tg WHERE \
   t.id = tg.tag_id AND tg.related_type = 1 AND tg.related_id = #{aid}")
  e = pg_new.exec_prepared("insert_entry",[
      a["title"],a["url"],"",a["summary"],a["markdown"],"t",a["created_at"],a["updated_at"]
    ])[0]
  eid = e["id"]
  ts.each do |t|
    tid = t["id"]
    pg_new.exec_prepared("insert_tagging",[eid,tid])
  end
  i = i + 1
end
