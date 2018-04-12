package utils;

import java.io.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class CompactAlgorithm {

    /**
     * 压缩单个文件
     *
     * @param srcfile
     */
    //压缩ZIP文件
    public void zipFile(File srcfile, ZipOutputStream out, String basedir) {
        byte[] buf = new byte[1024];
        FileInputStream in = null;

        try {
            int len;
            in = new FileInputStream(srcfile);
            out.putNextEntry(new ZipEntry(basedir + srcfile.getName()));

            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (out != null)
                    out.closeEntry();
                if (in != null)
                    in.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    //解压GZ压缩文件
    public static void unGzipFile(String sourcedir,String ouputfile) {
        try {
            //建立gzip压缩文件输入流
            FileInputStream fin = new FileInputStream(sourcedir);
            //建立gzip解压工作流
            GZIPInputStream gzin = new GZIPInputStream(fin);
            //建立解压文件输出流
/*            ouputfile = sourcedir.substring(0,sourcedir.lastIndexOf('.'));
            ouputfile = ouputfile.substring(0,ouputfile.lastIndexOf('.'));*/
            FileOutputStream fout = new FileOutputStream(ouputfile);

            int num;
            byte[] buf=new byte[1024];

            while ((num = gzin.read(buf,0,buf.length)) != -1)
            {
                fout.write(buf,0,num);
            }

            gzin.close();
            fout.close();
            fin.close();
        } catch (Exception ex){
            System.err.println(ex.toString());
        }
        return;
    }


    //测试
    public static void main(String[] args) {
        String gz = "D:/raw.split.A1.1.fq.gz";
        String fq = "D:/xx.fastq";
        unGzipFile(gz,fq);
    }
}
