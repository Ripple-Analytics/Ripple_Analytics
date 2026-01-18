import os
import logging
import asyncio
import aiohttp
import time
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Iterator
from datetime import datetime
from enum import Enum

logger = logging.getLogger(__name__)


class OCRProvider(Enum):
    LOCAL_TESSERACT = "tesseract"
    LOCAL_PYMUPDF = "pymupdf"
    ABBYY_CLOUD = "abbyy_cloud"
    ABBYY_FINEREADER = "abbyy_finereader"


@dataclass
class PDFPage:
    page_number: int
    content: str
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PDFDocument:
    path: str
    title: str
    author: str
    pages: List[PDFPage]
    total_pages: int
    total_chars: int
    metadata: Dict[str, Any] = field(default_factory=dict)
    extracted_at: datetime = field(default_factory=datetime.now)


class PDFProcessor:
    def __init__(
        self,
        extract_images: bool = False,
        ocr_enabled: bool = False,
        ocr_language: str = "eng",
    ):
        self.extract_images = extract_images
        self.ocr_enabled = ocr_enabled
        self.ocr_language = ocr_language
        self._pymupdf_available = self._check_pymupdf()
        self._pdfplumber_available = self._check_pdfplumber()
        self._pypdf_available = self._check_pypdf()

    def _check_pymupdf(self) -> bool:
        try:
            import fitz
            return True
        except ImportError:
            return False

    def _check_pdfplumber(self) -> bool:
        try:
            import pdfplumber
            return True
        except ImportError:
            return False

    def _check_pypdf(self) -> bool:
        try:
            import pypdf
            return True
        except ImportError:
            try:
                import PyPDF2
                return True
            except ImportError:
                return False

    def extract_text(self, path: Path) -> Optional[PDFDocument]:
        if self._pymupdf_available:
            return self._extract_with_pymupdf(path)
        elif self._pdfplumber_available:
            return self._extract_with_pdfplumber(path)
        elif self._pypdf_available:
            return self._extract_with_pypdf(path)
        else:
            logger.error("No PDF library available. Install pymupdf, pdfplumber, or pypdf.")
            return None

    def _extract_with_pymupdf(self, path: Path) -> Optional[PDFDocument]:
        try:
            import fitz
            
            doc = fitz.open(str(path))
            pages = []
            total_chars = 0
            
            metadata = {
                "producer": doc.metadata.get("producer", ""),
                "creator": doc.metadata.get("creator", ""),
                "creation_date": doc.metadata.get("creationDate", ""),
                "modification_date": doc.metadata.get("modDate", ""),
            }
            
            for page_num in range(len(doc)):
                page = doc[page_num]
                text = page.get_text("text")
                
                if not text.strip() and self.ocr_enabled:
                    text = self._ocr_page(page)
                
                pages.append(PDFPage(
                    page_number=page_num + 1,
                    content=text,
                    metadata={"width": page.rect.width, "height": page.rect.height}
                ))
                total_chars += len(text)
            
            doc.close()
            
            return PDFDocument(
                path=str(path),
                title=doc.metadata.get("title", path.stem),
                author=doc.metadata.get("author", ""),
                pages=pages,
                total_pages=len(pages),
                total_chars=total_chars,
                metadata=metadata,
            )
            
        except Exception as e:
            logger.error(f"PyMuPDF extraction failed for {path}: {e}")
            return None

    def _extract_with_pdfplumber(self, path: Path) -> Optional[PDFDocument]:
        try:
            import pdfplumber
            
            pages = []
            total_chars = 0
            metadata = {}
            
            with pdfplumber.open(str(path)) as pdf:
                metadata = pdf.metadata or {}
                
                for page_num, page in enumerate(pdf.pages):
                    text = page.extract_text() or ""
                    
                    pages.append(PDFPage(
                        page_number=page_num + 1,
                        content=text,
                        metadata={"width": page.width, "height": page.height}
                    ))
                    total_chars += len(text)
            
            return PDFDocument(
                path=str(path),
                title=metadata.get("Title", path.stem),
                author=metadata.get("Author", ""),
                pages=pages,
                total_pages=len(pages),
                total_chars=total_chars,
                metadata=metadata,
            )
            
        except Exception as e:
            logger.error(f"pdfplumber extraction failed for {path}: {e}")
            return None

    def _extract_with_pypdf(self, path: Path) -> Optional[PDFDocument]:
        try:
            try:
                from pypdf import PdfReader
            except ImportError:
                from PyPDF2 import PdfReader
            
            reader = PdfReader(str(path))
            pages = []
            total_chars = 0
            
            metadata = {}
            if reader.metadata:
                metadata = {
                    "title": reader.metadata.get("/Title", ""),
                    "author": reader.metadata.get("/Author", ""),
                    "creator": reader.metadata.get("/Creator", ""),
                    "producer": reader.metadata.get("/Producer", ""),
                }
            
            for page_num, page in enumerate(reader.pages):
                text = page.extract_text() or ""
                
                pages.append(PDFPage(
                    page_number=page_num + 1,
                    content=text,
                    metadata={}
                ))
                total_chars += len(text)
            
            return PDFDocument(
                path=str(path),
                title=metadata.get("title", path.stem),
                author=metadata.get("author", ""),
                pages=pages,
                total_pages=len(pages),
                total_chars=total_chars,
                metadata=metadata,
            )
            
        except Exception as e:
            logger.error(f"pypdf extraction failed for {path}: {e}")
            return None

    def _ocr_page(self, page) -> str:
        if not self.ocr_enabled:
            return ""
        
        try:
            import pytesseract
            from PIL import Image
            import io
            
            pix = page.get_pixmap(matrix=fitz.Matrix(2, 2))
            img_data = pix.tobytes("png")
            img = Image.open(io.BytesIO(img_data))
            
            text = pytesseract.image_to_string(img, lang=self.ocr_language)
            return text
            
        except Exception as e:
            logger.warning(f"OCR failed: {e}")
            return ""

    def extract_text_streaming(self, path: Path) -> Iterator[PDFPage]:
        if self._pymupdf_available:
            yield from self._stream_with_pymupdf(path)
        elif self._pdfplumber_available:
            yield from self._stream_with_pdfplumber(path)
        else:
            doc = self.extract_text(path)
            if doc:
                yield from doc.pages

    def _stream_with_pymupdf(self, path: Path) -> Iterator[PDFPage]:
        try:
            import fitz
            
            doc = fitz.open(str(path))
            
            for page_num in range(len(doc)):
                page = doc[page_num]
                text = page.get_text("text")
                
                if not text.strip() and self.ocr_enabled:
                    text = self._ocr_page(page)
                
                yield PDFPage(
                    page_number=page_num + 1,
                    content=text,
                    metadata={"width": page.rect.width, "height": page.rect.height}
                )
            
            doc.close()
            
        except Exception as e:
            logger.error(f"PyMuPDF streaming failed for {path}: {e}")

    def _stream_with_pdfplumber(self, path: Path) -> Iterator[PDFPage]:
        try:
            import pdfplumber
            
            with pdfplumber.open(str(path)) as pdf:
                for page_num, page in enumerate(pdf.pages):
                    text = page.extract_text() or ""
                    
                    yield PDFPage(
                        page_number=page_num + 1,
                        content=text,
                        metadata={"width": page.width, "height": page.height}
                    )
                    
        except Exception as e:
            logger.error(f"pdfplumber streaming failed for {path}: {e}")

    def get_page_count(self, path: Path) -> int:
        if self._pymupdf_available:
            try:
                import fitz
                doc = fitz.open(str(path))
                count = len(doc)
                doc.close()
                return count
            except Exception:
                pass
        
        if self._pdfplumber_available:
            try:
                import pdfplumber
                with pdfplumber.open(str(path)) as pdf:
                    return len(pdf.pages)
            except Exception:
                pass
        
        if self._pypdf_available:
            try:
                try:
                    from pypdf import PdfReader
                except ImportError:
                    from PyPDF2 import PdfReader
                reader = PdfReader(str(path))
                return len(reader.pages)
            except Exception:
                pass
        
        return 0

    def is_available(self) -> bool:
        return self._pymupdf_available or self._pdfplumber_available or self._pypdf_available

    def get_available_backends(self) -> List[str]:
        backends = []
        if self._pymupdf_available:
            backends.append("pymupdf")
        if self._pdfplumber_available:
            backends.append("pdfplumber")
        if self._pypdf_available:
            backends.append("pypdf")
        return backends


class ABBYYFineReaderClient:
    def __init__(
        self,
        application_id: str = None,
        password: str = None,
        service_url: str = "https://cloud-westus.ocrsdk.com",
    ):
        self.application_id = application_id or os.getenv("ABBYY_APPLICATION_ID", "")
        self.password = password or os.getenv("ABBYY_PASSWORD", "")
        self.service_url = service_url
        self._session: Optional[aiohttp.ClientSession] = None

    async def _get_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            auth = aiohttp.BasicAuth(self.application_id, self.password)
            self._session = aiohttp.ClientSession(auth=auth)
        return self._session

    async def close(self):
        if self._session and not self._session.closed:
            await self._session.close()

    def is_configured(self) -> bool:
        return bool(self.application_id and self.password)

    async def process_image(
        self,
        file_path: Path,
        language: str = "English",
        export_format: str = "txt",
    ) -> Optional[str]:
        if not self.is_configured():
            logger.error("ABBYY credentials not configured")
            return None

        session = await self._get_session()
        
        try:
            with open(file_path, "rb") as f:
                file_data = f.read()

            params = {
                "language": language,
                "exportFormat": export_format,
            }

            async with session.post(
                f"{self.service_url}/processImage",
                params=params,
                data=file_data,
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    logger.error(f"ABBYY processImage failed: {response.status} - {error_text}")
                    return None

                result = await response.text()
                import xml.etree.ElementTree as ET
                root = ET.fromstring(result)
                task_id = root.get("id")
                
                if not task_id:
                    logger.error("No task ID returned from ABBYY")
                    return None

                return await self._wait_for_task(task_id)

        except Exception as e:
            logger.error(f"ABBYY process_image failed: {e}")
            return None

    async def process_document(
        self,
        file_path: Path,
        language: str = "English",
        export_format: str = "txt",
        profile: str = "documentConversion",
    ) -> Optional[str]:
        if not self.is_configured():
            logger.error("ABBYY credentials not configured")
            return None

        session = await self._get_session()
        
        try:
            with open(file_path, "rb") as f:
                file_data = f.read()

            params = {
                "language": language,
                "exportFormat": export_format,
                "profile": profile,
            }

            async with session.post(
                f"{self.service_url}/processDocument",
                params=params,
                data=file_data,
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    logger.error(f"ABBYY processDocument failed: {response.status} - {error_text}")
                    return None

                result = await response.text()
                import xml.etree.ElementTree as ET
                root = ET.fromstring(result)
                task_id = root.get("id")
                
                if not task_id:
                    logger.error("No task ID returned from ABBYY")
                    return None

                return await self._wait_for_task(task_id)

        except Exception as e:
            logger.error(f"ABBYY process_document failed: {e}")
            return None

    async def _wait_for_task(
        self,
        task_id: str,
        max_wait_seconds: int = 300,
        poll_interval: float = 2.0,
    ) -> Optional[str]:
        session = await self._get_session()
        start_time = time.time()

        while time.time() - start_time < max_wait_seconds:
            try:
                async with session.get(
                    f"{self.service_url}/getTaskStatus",
                    params={"taskId": task_id},
                ) as response:
                    if response.status != 200:
                        logger.error(f"ABBYY getTaskStatus failed: {response.status}")
                        return None

                    result = await response.text()
                    import xml.etree.ElementTree as ET
                    root = ET.fromstring(result)
                    status = root.get("status")

                    if status == "Completed":
                        result_url = root.get("resultUrl")
                        if result_url:
                            return await self._download_result(result_url)
                        return None

                    elif status in ("ProcessingFailed", "NotEnoughCredits", "Deleted"):
                        logger.error(f"ABBYY task failed with status: {status}")
                        return None

                    await asyncio.sleep(poll_interval)

            except Exception as e:
                logger.error(f"ABBYY _wait_for_task failed: {e}")
                return None

        logger.error(f"ABBYY task timed out after {max_wait_seconds} seconds")
        return None

    async def _download_result(self, result_url: str) -> Optional[str]:
        session = await self._get_session()
        
        try:
            async with session.get(result_url) as response:
                if response.status != 200:
                    logger.error(f"ABBYY download failed: {response.status}")
                    return None
                return await response.text()

        except Exception as e:
            logger.error(f"ABBYY _download_result failed: {e}")
            return None

    async def get_application_info(self) -> Optional[Dict[str, Any]]:
        if not self.is_configured():
            return None

        session = await self._get_session()
        
        try:
            async with session.get(f"{self.service_url}/getApplicationInfo") as response:
                if response.status != 200:
                    return None

                result = await response.text()
                import xml.etree.ElementTree as ET
                root = ET.fromstring(result)
                
                return {
                    "name": root.get("name"),
                    "pages": int(root.get("pages", 0)),
                    "credits": int(root.get("credits", 0)),
                    "type": root.get("type"),
                }

        except Exception as e:
            logger.error(f"ABBYY get_application_info failed: {e}")
            return None

    async def list_tasks(self, from_date: str = None, to_date: str = None) -> List[Dict[str, Any]]:
        if not self.is_configured():
            return []

        session = await self._get_session()
        params = {}
        if from_date:
            params["fromDate"] = from_date
        if to_date:
            params["toDate"] = to_date

        try:
            async with session.get(
                f"{self.service_url}/listTasks",
                params=params,
            ) as response:
                if response.status != 200:
                    return []

                result = await response.text()
                import xml.etree.ElementTree as ET
                root = ET.fromstring(result)
                
                tasks = []
                for task in root.findall("task"):
                    tasks.append({
                        "id": task.get("id"),
                        "status": task.get("status"),
                        "registrationTime": task.get("registrationTime"),
                        "statusChangeTime": task.get("statusChangeTime"),
                        "filesCount": int(task.get("filesCount", 0)),
                        "credits": int(task.get("credits", 0)),
                    })
                return tasks

        except Exception as e:
            logger.error(f"ABBYY list_tasks failed: {e}")
            return []


class EnhancedPDFProcessor(PDFProcessor):
    def __init__(
        self,
        extract_images: bool = False,
        ocr_enabled: bool = False,
        ocr_language: str = "eng",
        ocr_provider: OCRProvider = OCRProvider.LOCAL_PYMUPDF,
        abbyy_application_id: str = None,
        abbyy_password: str = None,
    ):
        super().__init__(extract_images, ocr_enabled, ocr_language)
        self.ocr_provider = ocr_provider
        self.abbyy_client: Optional[ABBYYFineReaderClient] = None
        
        if ocr_provider in (OCRProvider.ABBYY_CLOUD, OCRProvider.ABBYY_FINEREADER):
            self.abbyy_client = ABBYYFineReaderClient(
                application_id=abbyy_application_id,
                password=abbyy_password,
            )

    async def extract_text_with_abbyy(
        self,
        path: Path,
        language: str = "English",
    ) -> Optional[PDFDocument]:
        if not self.abbyy_client or not self.abbyy_client.is_configured():
            logger.warning("ABBYY not configured, falling back to local extraction")
            return self.extract_text(path)

        text = await self.abbyy_client.process_document(
            path,
            language=language,
            export_format="txt",
        )

        if not text:
            logger.warning(f"ABBYY extraction failed for {path}, falling back to local")
            return self.extract_text(path)

        pages = [PDFPage(
            page_number=1,
            content=text,
            metadata={"ocr_provider": "abbyy"},
        )]

        return PDFDocument(
            path=str(path),
            title=path.stem,
            author="",
            pages=pages,
            total_pages=1,
            total_chars=len(text),
            metadata={"ocr_provider": "abbyy", "language": language},
        )

    async def extract_text_auto(
        self,
        path: Path,
        prefer_abbyy: bool = True,
        abbyy_language: str = "English",
    ) -> Optional[PDFDocument]:
        local_doc = self.extract_text(path)
        
        if local_doc and local_doc.total_chars > 100:
            return local_doc

        if prefer_abbyy and self.abbyy_client and self.abbyy_client.is_configured():
            logger.info(f"Local extraction yielded little text, trying ABBYY for {path}")
            return await self.extract_text_with_abbyy(path, abbyy_language)

        return local_doc

    async def close(self):
        if self.abbyy_client:
            await self.abbyy_client.close()

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()
        return False
