use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn ast(c: &mut Criterion) {
    let source = include_str!("complex-tree.dts");

    c.bench_function("parser::from_str complex-tree.dts", |b| {
        b.iter(|| (black_box(source)))
    });
}

criterion_group!(benches, ast);
criterion_main!(benches);
