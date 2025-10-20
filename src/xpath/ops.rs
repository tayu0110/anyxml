use crate::xpath::{XPathError, XPathObject};

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

    pub(super) fn eq(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::Number(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::String(left), XPathObject::String(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
        }
    }

    pub(super) fn ne(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::Number(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::String(left), XPathObject::String(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
        }
    }

    pub(super) fn lt(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (left, right) => {
                let left = left.as_number()?;
                let right = right.as_number()?;
                Ok((left < right).into())
            }
        }
    }

    pub(super) fn le(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
            (left, right) => {
                let left = left.as_number()?;
                let right = right.as_number()?;
                Ok((left <= right).into())
            }
        }
    }

    pub(super) fn gt(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::Number(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::String(left), XPathObject::String(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
        }
    }

    pub(super) fn ge(self, other: Self) -> Result<Self, XPathError> {
        match (self, other) {
            (XPathObject::Number(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Number(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::String(left), XPathObject::String(right)) => todo!(),
            (XPathObject::String(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::String(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::String(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::Boolean(left), XPathObject::NodeSet(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Number(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::String(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::Boolean(right)) => todo!(),
            (XPathObject::NodeSet(left), XPathObject::NodeSet(right)) => todo!(),
        }
    }
}
