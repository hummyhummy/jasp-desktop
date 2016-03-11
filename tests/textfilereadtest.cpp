#include "textfilereadtest.h"


void TextFileReadTest::initTestCase()
{

}

void TextFileReadTest::cleanupTestCase()
{

}

void TextFileReadTest::init()
{
  fe = new FileEvent();
  dsp = new DataSetPackage();
  asl = new AsyncLoader();
}

void TextFileReadTest::cleanup()
{
  // destroy all the objects created and delete the dataSet from the shared memory
  SharedMemory::deleteDataSet(dsp->dataSet);

  fe->~FileEvent();
  dsp->~DataSetPackage();
  asl->~AsyncLoader();
}

void TextFileReadTest::asyncloaderTester1()
{
  std::string _path = "testfile1.txt";

  bool wasBlocked = fe->blockSignals(true); //block all signals emitted by the FileEvent object
  fe->setOperation(FileEvent::FileOpen);
  fe->setPath(QString::fromStdString(_path));

  wasBlocked = asl->blockSignals(true);  //block all signals emitted by the Asyncloader object
  asl->loadTask(fe, dsp);
  asl->_thread.quit();

  struct fileContent fc;
  readDataFromFile(_path, &fc);

  QVERIFY(checkIfEqual(&fc)); //test the opening and reading of text files
}


void TextFileReadTest::asyncloaderTester2()
{
  std::string _path = "testfile2.txt";
  bool wasBlocked = fe->blockSignals(true);
  fe->setOperation(FileEvent::FileOpen);
  fe->setPath(QString::fromStdString(_path));

  wasBlocked = asl->blockSignals(true);
  asl->loadTask(fe, dsp);
  asl->_thread.quit();

  struct fileContent fc;
  readDataFromFile(_path, &fc);

  bool ans = checkIfEqual(&fc);

  QVERIFY(ans);
}


/* checks if data read from file is same as the data stored in the shared memory */
bool TextFileReadTest::checkIfEqual(struct fileContent *fc)
{
  if(fc->columns != dsp->dataSet->columnCount())
  {
    return false;
  }

  if(fc->rows != dsp->dataSet->rowCount())
  {
    return false;
  }

  for(int i=0; i<fc->columns; ++i)
  {
    if(fc->headers[i] != dsp->dataSet->column(i).name())
    {
      return false;
    }

    for(int j=0; j<fc->rows; ++j)
    {
      if(fc->data[j][i] != dsp->dataSet->column(i)[j])
      {
        return false;
      }
    }
  }

  return true;
}

/* read data from the file specified from path and store it in the struct fileContent */
void TextFileReadTest::readDataFromFile(std::string path, struct fileContent *fc)
{
  std::ifstream input(path.c_str());
  std::vector< std::vector<std::string> > fileRows;
  
  int numCols = 0;
  int numRows = 0;
  char delimiter = '\t';

  if(input.is_open())
  {
    std::string line; //line from the file
    std::string currentWord;
    std::vector<std::string> tempRow;

    std::getline(input, line);
    std::size_t found = line.find(delimiter);

    if(found == std::string::npos) // tab is not found, separater is space character
    {
      delimiter = ' ';
    }

    std::istringstream buffer(line);

    while(std::getline(buffer, currentWord, delimiter)) //separate with respect to the delimiter
    {
      numCols++;
      tempRow.push_back(currentWord);
    }

    fc->columns = numCols;
    fc->headers = tempRow;
    buffer.clear();

    while(std::getline(input, line))
    {
      numRows++;
      buffer.str(line);
      for(int i=0; i<numCols; ++i)
      {
        std::getline(buffer, currentWord, delimiter);
        tempRow.push_back(currentWord);
      }

      fileRows.push_back(tempRow);
      tempRow.clear();
      buffer.clear();
    }

    fc->rows = numRows;
    fc->data = fileRows;
  }
  else
  {
    qDebug() << "File open failed";
  }
}
