use super::*;

#[test]
fn path_prefix() {
    let mut file_set = FileSetConfig::builder();
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo".into())]);
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo/bar/baz".into())]);
    let file_set = file_set.build();

    let files = [
        (FileId::from_raw(0), VfsPath::new_virtual_path("/foo/src/lib.rs".into())),
        (FileId::from_raw(1), VfsPath::new_virtual_path("/foo/src/bar/baz/lib.rs".into())),
        (FileId::from_raw(2), VfsPath::new_virtual_path("/foo/bar/baz/lib.rs".into())),
        (FileId::from_raw(3), VfsPath::new_virtual_path("/quux/lib.rs".into())),
    ];

    let partition = file_set.partition(files).into_iter().map(|it| it.len()).collect::<Vec<_>>();
    assert_eq!(partition, vec![2, 1, 1]);
}

#[test]
fn name_prefix() {
    let mut file_set = FileSetConfig::builder();
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo".into())]);
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo-things".into())]);
    let file_set = file_set.build();

    let files = [
        (FileId::from_raw(0), VfsPath::new_virtual_path("/foo/src/lib.rs".into())),
        (FileId::from_raw(1), VfsPath::new_virtual_path("/foo-things/src/lib.rs".into())),
    ];

    let partition = file_set.partition(files).into_iter().map(|it| it.len()).collect::<Vec<_>>();
    assert_eq!(partition, vec![1, 1, 0]);
}

/// Ensure that we don't consider `/foo/bar_baz.rs` to be in the
/// `/foo/bar/` root.
#[test]
fn name_prefix_partially_matches() {
    let mut file_set = FileSetConfig::builder();
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo".into())]);
    file_set.add_file_set(vec![VfsPath::new_virtual_path("/foo/bar".into())]);
    let file_set = file_set.build();

    let files = [
        // These two are both in /foo.
        (FileId::from_raw(0), VfsPath::new_virtual_path("/foo/lib.rs".into())),
        (FileId::from_raw(1), VfsPath::new_virtual_path("/foo/bar_baz.rs".into())),
        // Only this file is in /foo/bar.
        (FileId::from_raw(2), VfsPath::new_virtual_path("/foo/bar/biz.rs".into())),
    ];

    let partition = file_set.partition(files).into_iter().map(|it| it.len()).collect::<Vec<_>>();

    assert_eq!(partition, vec![2, 1, 0]);
}
