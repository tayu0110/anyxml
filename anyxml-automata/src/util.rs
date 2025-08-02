use crate::Atom;

pub fn normalize_ranges<A: Atom>(
    iter: impl Iterator<Item = (A, A)>,
) -> impl Iterator<Item = (A, A)> {
    let mut buf = iter.collect::<Vec<_>>();
    buf.sort_unstable();

    let mut ret = vec![];
    for (start, end) in buf {
        assert!(start <= end);
        match ret.last_mut() {
            Some((_, e)) if start <= *e || Some(start) == e.next() => {
                *e = end;
            }
            _ => ret.push((start, end)),
        }
    }

    ret.into_iter()
}

/// perform `iter | other`
pub fn union_ranges<A: Atom>(
    iter: impl Iterator<Item = (A, A)>,
    other: impl Iterator<Item = (A, A)>,
) -> impl Iterator<Item = (A, A)> {
    normalize_ranges(iter.chain(other))
}

/// performe `not iter`
pub fn complement_ranges<A: Atom>(
    iter: impl Iterator<Item = (A, A)>,
) -> impl Iterator<Item = (A, A)> {
    let mut iter = normalize_ranges(iter);
    let mut prev = Some(A::MIN);
    std::iter::from_fn(move || {
        for (end, next) in iter.by_ref() {
            let start = prev?;
            prev = next.next();
            if let Some(end) = end.previous().filter(|end| &start <= end) {
                return Some((start, end));
            }
        }
        prev.take().map(|start| (start, A::MAX))
    })
}

/// perform `iter - other`
pub fn difference_ranges<A: Atom>(
    iter: impl Iterator<Item = (A, A)>,
    other: impl Iterator<Item = (A, A)>,
) -> impl Iterator<Item = (A, A)> {
    let mut iter = normalize_ranges(iter);
    let mut other = normalize_ranges(other);

    let mut next = iter.next();
    let mut bad = other.next();
    std::iter::from_fn(move || {
        let (mut start, mut end) = next?;

        while let Some((bs, be)) = bad {
            if be < start {
                bad = other.next();
            } else if end < bs {
                next = iter.next();
                return Some((start, end));
            } else if bs <= start {
                if end <= be {
                    // bs - start - end - be
                    next = iter.next();
                    (start, end) = next?;
                } else {
                    // bs - start - be - end
                    start = be.next().unwrap();
                    bad = other.next();
                }
            } else if end <= be {
                // start - bs - end - be
                next = iter.next();
                return Some((start, bs.previous().unwrap()));
            } else {
                // start - bs - be - end
                next = Some((be.next().unwrap(), end));
                bad = other.next();
                return Some((start, bs.previous().unwrap()));
            }
        }
        Some((start, end))
    })
}
