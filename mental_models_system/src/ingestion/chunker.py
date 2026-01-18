from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Iterator
import re


class ChunkingStrategy(Enum):
    FIXED_SIZE = "fixed_size"
    SENTENCE = "sentence"
    PARAGRAPH = "paragraph"
    SEMANTIC = "semantic"
    SLIDING_WINDOW = "sliding_window"


@dataclass
class TextChunk:
    content: str
    start_pos: int
    end_pos: int
    chunk_index: int
    metadata: dict = field(default_factory=dict)


class TextChunker:
    def __init__(
        self,
        strategy: ChunkingStrategy = ChunkingStrategy.SLIDING_WINDOW,
        chunk_size: int = 1000,
        chunk_overlap: int = 200,
        min_chunk_size: int = 100,
    ):
        self.strategy = strategy
        self.chunk_size = chunk_size
        self.chunk_overlap = chunk_overlap
        self.min_chunk_size = min_chunk_size
        
        self._sentence_pattern = re.compile(r'(?<=[.!?])\s+(?=[A-Z])')
        self._paragraph_pattern = re.compile(r'\n\s*\n')

    def chunk(self, text: str, metadata: Optional[dict] = None) -> List[TextChunk]:
        if not text or not text.strip():
            return []
        
        metadata = metadata or {}
        
        if self.strategy == ChunkingStrategy.FIXED_SIZE:
            return self._chunk_fixed_size(text, metadata)
        elif self.strategy == ChunkingStrategy.SENTENCE:
            return self._chunk_by_sentence(text, metadata)
        elif self.strategy == ChunkingStrategy.PARAGRAPH:
            return self._chunk_by_paragraph(text, metadata)
        elif self.strategy == ChunkingStrategy.SLIDING_WINDOW:
            return self._chunk_sliding_window(text, metadata)
        elif self.strategy == ChunkingStrategy.SEMANTIC:
            return self._chunk_semantic(text, metadata)
        else:
            return self._chunk_sliding_window(text, metadata)

    def _chunk_fixed_size(self, text: str, metadata: dict) -> List[TextChunk]:
        chunks = []
        pos = 0
        chunk_index = 0
        
        while pos < len(text):
            end_pos = min(pos + self.chunk_size, len(text))
            chunk_text = text[pos:end_pos]
            
            if len(chunk_text.strip()) >= self.min_chunk_size:
                chunks.append(TextChunk(
                    content=chunk_text,
                    start_pos=pos,
                    end_pos=end_pos,
                    chunk_index=chunk_index,
                    metadata={**metadata, "strategy": "fixed_size"}
                ))
                chunk_index += 1
            
            pos = end_pos
        
        return chunks

    def _chunk_sliding_window(self, text: str, metadata: dict) -> List[TextChunk]:
        chunks = []
        pos = 0
        chunk_index = 0
        
        while pos < len(text):
            end_pos = min(pos + self.chunk_size, len(text))
            
            if end_pos < len(text):
                break_pos = self._find_break_point(text, end_pos)
                if break_pos > pos + self.min_chunk_size:
                    end_pos = break_pos
            
            chunk_text = text[pos:end_pos]
            
            if len(chunk_text.strip()) >= self.min_chunk_size:
                chunks.append(TextChunk(
                    content=chunk_text.strip(),
                    start_pos=pos,
                    end_pos=end_pos,
                    chunk_index=chunk_index,
                    metadata={**metadata, "strategy": "sliding_window"}
                ))
                chunk_index += 1
            
            step = max(self.chunk_size - self.chunk_overlap, self.min_chunk_size)
            pos += step
            
            if pos >= len(text) - self.min_chunk_size and pos < len(text):
                break
        
        return chunks

    def _chunk_by_sentence(self, text: str, metadata: dict) -> List[TextChunk]:
        sentences = self._sentence_pattern.split(text)
        chunks = []
        current_chunk = ""
        current_start = 0
        chunk_index = 0
        pos = 0
        
        for sentence in sentences:
            sentence = sentence.strip()
            if not sentence:
                continue
            
            if len(current_chunk) + len(sentence) + 1 <= self.chunk_size:
                if current_chunk:
                    current_chunk += " " + sentence
                else:
                    current_chunk = sentence
                    current_start = pos
            else:
                if current_chunk and len(current_chunk) >= self.min_chunk_size:
                    chunks.append(TextChunk(
                        content=current_chunk,
                        start_pos=current_start,
                        end_pos=pos,
                        chunk_index=chunk_index,
                        metadata={**metadata, "strategy": "sentence"}
                    ))
                    chunk_index += 1
                
                current_chunk = sentence
                current_start = pos
            
            pos += len(sentence) + 1
        
        if current_chunk and len(current_chunk) >= self.min_chunk_size:
            chunks.append(TextChunk(
                content=current_chunk,
                start_pos=current_start,
                end_pos=len(text),
                chunk_index=chunk_index,
                metadata={**metadata, "strategy": "sentence"}
            ))
        
        return chunks

    def _chunk_by_paragraph(self, text: str, metadata: dict) -> List[TextChunk]:
        paragraphs = self._paragraph_pattern.split(text)
        chunks = []
        current_chunk = ""
        current_start = 0
        chunk_index = 0
        pos = 0
        
        for para in paragraphs:
            para = para.strip()
            if not para:
                continue
            
            if len(current_chunk) + len(para) + 2 <= self.chunk_size:
                if current_chunk:
                    current_chunk += "\n\n" + para
                else:
                    current_chunk = para
                    current_start = pos
            else:
                if current_chunk and len(current_chunk) >= self.min_chunk_size:
                    chunks.append(TextChunk(
                        content=current_chunk,
                        start_pos=current_start,
                        end_pos=pos,
                        chunk_index=chunk_index,
                        metadata={**metadata, "strategy": "paragraph"}
                    ))
                    chunk_index += 1
                
                if len(para) > self.chunk_size:
                    sub_chunks = self._chunk_sliding_window(para, metadata)
                    for sub_chunk in sub_chunks:
                        sub_chunk.chunk_index = chunk_index
                        sub_chunk.start_pos += pos
                        sub_chunk.end_pos += pos
                        chunks.append(sub_chunk)
                        chunk_index += 1
                    current_chunk = ""
                else:
                    current_chunk = para
                    current_start = pos
            
            pos += len(para) + 2
        
        if current_chunk and len(current_chunk) >= self.min_chunk_size:
            chunks.append(TextChunk(
                content=current_chunk,
                start_pos=current_start,
                end_pos=len(text),
                chunk_index=chunk_index,
                metadata={**metadata, "strategy": "paragraph"}
            ))
        
        return chunks

    def _chunk_semantic(self, text: str, metadata: dict) -> List[TextChunk]:
        return self._chunk_by_paragraph(text, metadata)

    def _find_break_point(self, text: str, pos: int) -> int:
        search_start = max(0, pos - 100)
        search_text = text[search_start:pos]
        
        for pattern in ['\n\n', '\n', '. ', '? ', '! ', '; ', ', ', ' ']:
            idx = search_text.rfind(pattern)
            if idx != -1:
                return search_start + idx + len(pattern)
        
        return pos

    def chunk_stream(self, text_iterator: Iterator[str], metadata: Optional[dict] = None) -> Iterator[TextChunk]:
        buffer = ""
        chunk_index = 0
        global_pos = 0
        metadata = metadata or {}
        
        for text_part in text_iterator:
            buffer += text_part
            
            while len(buffer) >= self.chunk_size + self.chunk_overlap:
                end_pos = self.chunk_size
                break_pos = self._find_break_point(buffer, end_pos)
                if break_pos > self.min_chunk_size:
                    end_pos = break_pos
                
                chunk_text = buffer[:end_pos].strip()
                
                if len(chunk_text) >= self.min_chunk_size:
                    yield TextChunk(
                        content=chunk_text,
                        start_pos=global_pos,
                        end_pos=global_pos + end_pos,
                        chunk_index=chunk_index,
                        metadata={**metadata, "strategy": "streaming"}
                    )
                    chunk_index += 1
                
                step = max(end_pos - self.chunk_overlap, self.min_chunk_size)
                buffer = buffer[step:]
                global_pos += step
        
        if buffer.strip() and len(buffer.strip()) >= self.min_chunk_size:
            yield TextChunk(
                content=buffer.strip(),
                start_pos=global_pos,
                end_pos=global_pos + len(buffer),
                chunk_index=chunk_index,
                metadata={**metadata, "strategy": "streaming"}
            )
