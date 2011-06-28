-record(upload, {id=none, name=none, description=none, created}).
-record(con, {key=none, io=none, path=none, total=0, done=0, upload=#upload{}}).
-record(id, {type, id}).


