pub struct LineBuf {
    cap: usize,
    top_idx: usize,
    buf: Vec<String>,
}

impl LineBuf {
    pub fn new(cap: usize) -> LineBuf {
        LineBuf {
            cap,
            top_idx: 0,
            buf: vec![],
        }
    }

    pub fn cap(&self) -> usize {
        self.cap
    }

    pub fn push(&mut self, line: String) {
        if self.buf.len() == self.cap {
            self.buf[self.top_idx] = line;
            self.top_idx += 1;
            if self.top_idx == self.cap {
                self.top_idx = 0;
            }
        }
    }
}
