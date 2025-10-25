use crate::xpath::{XPathError, XPathNodeSet, XPathObject};

impl XPathObject {
    pub(super) fn add(self, other: Self) -> Result<Self, XPathError> {
        Ok((self.as_number()? + other.as_number()?).into())
    }

    pub(super) fn sub(self, other: Self) -> Result<Self, XPathError> {
        Ok((self.as_number()? - other.as_number()?).into())
    }

    pub(super) fn mul(self, other: Self) -> Result<Self, XPathError> {
        Ok((self.as_number()? * other.as_number()?).into())
    }

    pub(super) fn div(self, other: Self) -> Result<Self, XPathError> {
        Ok((self.as_number()? / other.as_number()?).into())
    }

    pub(super) fn rem(self, other: Self) -> Result<Self, XPathError> {
        Ok((self.as_number()? % other.as_number()?).into())
    }

    pub(super) fn neg(self) -> Result<Self, XPathError> {
        Ok((-self.as_number()?).into())
    }

    fn equality(self, other: Self, neg: bool) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => {
                for left in &left {
                    let left = left.xpath_string_value();
                    if right
                        .iter()
                        .any(|right| (left == right.xpath_string_value()) != neg)
                    {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (XPathObject::NodeSet(left), XPathObject::Number(right))
            | (XPathObject::Number(right), XPathObject::NodeSet(left)) => {
                for node in &left {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(node);
                    let object = XPathObject::from(node_set);
                    if object.as_number().is_ok_and(|num| (num == right) != neg) {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (XPathObject::NodeSet(left), XPathObject::String(right))
            | (XPathObject::String(right), XPathObject::NodeSet(left)) => {
                for node in &left {
                    if (node.xpath_string_value().as_str() == right.as_ref()) != neg {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (XPathObject::NodeSet(left), XPathObject::Boolean(right))
            | (XPathObject::Boolean(right), XPathObject::NodeSet(left)) => {
                for node in &left {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(node);
                    let object = XPathObject::from(node_set);
                    if object.as_boolean().is_ok_and(|num| (num == right) != neg) {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (XPathObject::Number(left), XPathObject::Number(right)) => {
                Ok(((left == right) != neg).into())
            }
            (XPathObject::Number(left), right @ XPathObject::String(_))
            | (right @ XPathObject::String(_), XPathObject::Number(left)) => {
                let right = right.as_number()?;
                Ok(((left == right) != neg).into())
            }
            (left @ XPathObject::Number(_), XPathObject::Boolean(right))
            | (XPathObject::Boolean(right), left @ XPathObject::Number(_)) => {
                let left = left.as_boolean()?;
                Ok(((left == right) != neg).into())
            }
            (XPathObject::String(left), XPathObject::String(right)) => {
                Ok(((left == right) != neg).into())
            }
            (left @ XPathObject::String(_), XPathObject::Boolean(right))
            | (XPathObject::Boolean(right), left @ XPathObject::String(_)) => {
                let left = left.as_boolean()?;
                Ok(((left == right) != neg).into())
            }
            (XPathObject::Boolean(left), XPathObject::Boolean(right)) => {
                Ok(((left == right) != neg).into())
            }
        }
    }

    pub(super) fn eq(self, other: Self) -> Result<Self, XPathError> {
        self.equality(other, false)
    }

    pub(super) fn ne(self, other: Self) -> Result<Self, XPathError> {
        self.equality(other, true)
    }

    fn compare(self, other: Self, comparater: fn(f64, f64) -> bool) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => {
                for left in &left {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(left);
                    if let Ok(left) = XPathObject::from(node_set).as_number()
                        && right
                            .iter()
                            .filter_map(|node| {
                                let mut node_set = XPathNodeSet::default();
                                node_set.push(node);
                                XPathObject::from(node_set).as_number().ok()
                            })
                            .any(|right| comparater(left, right))
                    {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (XPathObject::NodeSet(left), right) => {
                let right = right.as_number()?;
                for node in &left {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(node);
                    let object = XPathObject::from(node_set);
                    if object.as_number().is_ok_and(|left| comparater(left, right)) {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (left, XPathObject::NodeSet(right)) => {
                let left = left.as_number()?;
                for node in &right {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(node);
                    let object = XPathObject::from(node_set);
                    if object
                        .as_number()
                        .is_ok_and(|right| comparater(left, right))
                    {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            (left, right) => {
                let left = left.as_number()?;
                let right = right.as_number()?;
                Ok(comparater(left, right).into())
            }
        }
    }

    pub(super) fn lt(self, other: Self) -> Result<Self, XPathError> {
        self.compare(other, |left, right| left < right)
    }

    pub(super) fn le(self, other: Self) -> Result<Self, XPathError> {
        self.compare(other, |left, right| left <= right)
    }

    pub(super) fn gt(self, other: Self) -> Result<Self, XPathError> {
        self.compare(other, |left, right| left > right)
    }

    pub(super) fn ge(self, other: Self) -> Result<Self, XPathError> {
        self.compare(other, |left, right| left >= right)
    }

    pub(super) fn union(self, other: Self) -> Result<Self, XPathError> {
        let (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) = (self, other) else {
            return Err(XPathError::IncorrectOperandType);
        };
        Ok(left.union(&right).into())
    }
}
