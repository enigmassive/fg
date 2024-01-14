use std::fs;

#[test]
fn test() {
    fs::read_dir("./tests/ok/").unwrap().for_each(|entry| {
        let entry = entry.unwrap();
        let name = entry.file_name().into_string().unwrap();
        let path = entry.path();

        let src = String::from_utf8(fs::read(path).unwrap()).unwrap();
        let mut dest = vec![];
        fg::compile(&src, &mut dest);
        let code = String::from_utf8(dest).unwrap();
        insta::assert_snapshot!(name, code);
    })
}
