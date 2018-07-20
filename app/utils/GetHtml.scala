package utils

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object GetHtml {


  def deploy(userId: Int, proId: Int, sampleId: Int, sample: String) = {
    val path = Utils.outPath(userId, proId, sampleId)
    val row = FileUtils.readLines(new File(path, "/deploy.txt")).asScala
    var html =
      s"""
         |
         |                                <div class="form-group" style="display: none;">
         |                                    <div class="col-sm-6 indent">
         |                                        <input name="proname" id="proname" class="form-control" value="${proId}" />
         |                                    </div>
         |                                </div>
         |
         |                                <div class="form-group">
         |                                    <label class="col-sm-12">样品名:</label>
         |                                    <div class="col-sm-6 indent">
         |                                        <input name="sample" id="sample" class="form-control" value="${sample}"  readonly="readonly"/>
         |                                    </div>
         |                                </div>
         |
         |                                <div class="form-group">
         |                                    <label class="col-sm-5">Trimmomatic PE(paired-end)(质控参数设置)</label>
         |
         |                                        <a id="down-1" style="margin-left: 2em">
         |                                            <i class="fa fa-arrow-down"></i>
         |                                        </a>
         |                                        <a id="up-1" style="margin-left: 2em;
         |                                            display: none">
         |                                            <i class="fa fa-arrow-up"></i>
         |                                        </a>
         |
         |                                </div>
         |
         |                                <div id="set-1" style="display: none" class="indent">
         |
         |                                    <div class="form-group">
         |                                          <p class="col-sm-7">
         |                                            Input FASTQ quality file scores encoding type:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="encondingType">
          """.stripMargin
    val type1 = if (row(1) == "-phred33") {
      """
        |            <option value="-phred64">Illumina 1.3-1.7 Phred+64</option>
        |            <option value="-phred33" selected>Illumina 1.8+ Phred+33</option>
      """.stripMargin
    } else {
      """
        |            <option value="-phred64" selected>Illumina 1.3-1.7 Phred+64</option>
        |            <option value="-phred33">Illumina 1.8+ Phred+33</option>
      """.stripMargin
    }
    html += type1
    html +=
      s"""
         |                                            </select>
         |                                        </div>
         |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">Perform initial ILLUMINACLIP step?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="stepMethod" onchange="stepChange(this)">
        """.stripMargin
    val step = if (row(2) == "no") {
      """
        |                                                     <option value="yes">Yes</option>
        |                                                      <option value="no" selected>No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>
        |
        |                                            <div id="stepValue" style="display: none;">
        |                                   <div class="form-group" >
        |                                            <p class="col-sm-7 indent">Adapter sequences to use:</p>
        |                                            <div class="col-sm-6 indent-1">
        |                                                <select class="form-control" name="adapter">
      """.stripMargin
    } else {
      """
        |                                                     <option value="yes" selected>Yes</option>
        |                                                      <option value="no" >No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>p
        |
        |                                            <div id="stepValue">
        |                                      <div class="form-group" >
        |                                            <p class="col-sm-7 indent">Adapter sequences to use:</p>
        |                                            <div class="col-sm-6 indent-1">
        |                                                <select class="form-control" name="adapter">
      """.stripMargin
    }

    html += step
    val adapter = if (row(3) == "TruSeq2-PE.fa") {
      """
        |                                                    <option value="TruSeq2-PE.fa" selected>
        |                                                        TruSeq2 (paired-ended, for Illumina GAII)</option>
        |                                                    <option value="TruSeq3-PE.fa">
        |                                                        TruSeq3 (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="TruSeq3-PE-2.fa">
        |                                                        TruSeq3 (additional seqs) (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="NexteraPE-PE.fa">Nextera (paired-ended)</option>
      """.stripMargin
    } else if (row(3) == "TruSeq3-PE.fa") {
      """
        |                                                    <option value="TruSeq2-PE.fa">
        |                                                        TruSeq2 (paired-ended, for Illumina GAII)</option>
        |                                                    <option value="TruSeq3-PE.fa" selected>
        |                                                        TruSeq3 (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="TruSeq3-PE-2.fa">
        |                                                        TruSeq3 (additional seqs) (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="NexteraPE-PE.fa">Nextera (paired-ended)</option>
      """.stripMargin
    } else if (row(3) == "TruSeq3-PE-2.fa") {
      """
        |                                                    <option value="TruSeq2-PE.fa">
        |                                                        TruSeq2 (paired-ended, for Illumina GAII)</option>
        |                                                    <option value="TruSeq3-PE.fa">
        |                                                        TruSeq3 (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="TruSeq3-PE-2.fa" selected>
        |                                                        TruSeq3 (additional seqs) (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="NexteraPE-PE.fa">Nextera (paired-ended)</option>
      """.stripMargin
    } else {
      """
        |                                                    <option value="TruSeq2-PE.fa">
        |                                                        TruSeq2 (paired-ended, for Illumina GAII)</option>
        |                                                    <option value="TruSeq3-PE.fa" selected>
        |                                                        TruSeq3 (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="TruSeq3-PE-2.fa">
        |                                                        TruSeq3 (additional seqs) (paired-ended, for MiSeq and HiSeq)</option>
        |                                                    <option value="NexteraPE-PE.fa" selected>Nextera (paired-ended)</option>
      """.stripMargin
    }

    html += adapter

    html +=
      s"""
         |                                                </select>
         |                                            </div>
         |                                        </div>
         |
        |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Maximum mismatch count which will still allow a full match to be performed:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="seed_mismatches" id="seed_mismatches" class="form-control" value="${row(4)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                How accurate the match between the two 'adapter ligated' reads must be for PE palin
         |                                                drome read alignment:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="palindrome_clip_threshold" id="palindrome_clip_threshold" class="form-control" value="${row(5)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                How accurate the match between any adapter etc. sequence must be against a read:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="simple_clip_threshold" id="simple_clip_threshold" class="form-control" value="${row(6)}" />
         |                                            </div>
         |                                        </div>
         |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">Perform Sliding window trimming (SLIDINGWINDOW)?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="trimMethod" onchange="trimChange(this)">
        """.stripMargin
    val trim = if (row(7) == "yes") {
      """
        |                                                <option value="yes" selected>Yes</option>
        |                                                        <option value="no">No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>
        |
        |                                            <div id="trimValue">
      """.stripMargin
    } else {
      """
        |                                                <option value="yes" >Yes</option>
        |                                                        <option value="no" selected>No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>
        |
        |                                            <div id="trimValue" style="display:none;">
      """.stripMargin
    }
    html += trim
    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Number of bases to average across:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="window_size" id="window_size" class="form-control" value="${row(8)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Average quality required:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="required_quality" id="required_quality" class="form-control" value="${row(9)}" />
         |                                            </div>
         |                                        </div>
         |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">Drop reads below a specified length (MINLEN)?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="minlenMethod" onchange="minlenChange(this)">
        """.stripMargin

    val min = if (row(10) == "yes") {
      """
        |                                                <option value="yes" selected>Yes</option>
        |                                                       <option value="no">No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>
        |
        |                                            <div id="minlenValue">
      """.stripMargin
    } else {
      """
        |                                                <option value="yes">Yes</option>
        |                                                       <option value="no" selected>No</option>
        |                                                    </select>
        |                                                </div>
        |                                            </div>
        |
        |                                            <div id="minlenValue" style="display:none;">
      """.stripMargin
    }
    html += min
    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Minimum length of reads to be kept:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="minlen" id="minlen" class="form-control" value="${row(11)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">
         |                                            Cut bases off the start of a read, if below a threshold quality (LEADING)?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="leadingMethod" onchange="leadingChange(this)">
         |""".stripMargin
    val lead = if (row(12) == "no") {
      """|                                                <option value="yes">Yes</option>
         |                                                <option value="no" selected>No</option>
         |                                            </select>
         |                                        </div>
         |                                    </div>
         |                                    <div id="leadingValue" style="display: none">
      """.stripMargin
    } else {
      """|                                                <option value="yes" selected>Yes</option>
         |                                                <option value="no">No</option>
         |                                            </select>
         |                                        </div>
         |                                    </div>
         |                                    <div id="leadingValue">
      """.stripMargin
    }
    html += lead
    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Minimum quality required to keep a base:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="leading" id="leading" class="form-control" value="${row(13)}" />
         |                                            </div>
         |                                        </div>
         |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">
         |                                            Cut bases off the end of a read, if below a threshold quality (TRAILING)?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="trailingMethod" onchange="trailingChange(this)">
        """.stripMargin
    val trail = if (row(14) == "yes") {
      """
        |                                                <option value="yes" selected>Yes</option>
        |                                                <option value="no">No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="trailingValue">
      """.stripMargin
    } else {
      """
        |                                                <option value="yes">Yes</option>
        |                                                <option value="no" selected>No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="trailingValue" style="display:none;">
      """.stripMargin
    }
    html += trail
    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Minimum quality required to keep a base:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="trailing" id="trailing" class="form-control" value="${row(15)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">Cut the read to a specified length (CROP):</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="cropMethod" onchange="cropChange(this)">
         |""".stripMargin
    val crop = if (row(16) == "no") {
      """
        |                                                <option value="yes">Yes</option>
        |                                                <option value="no" selected>No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="cropValue" style="display: none">
      """.stripMargin
    } else {
      """
        |                                                <option value="yes" selected>Yes</option>
        |                                                <option value="no">No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="cropValue">
      """.stripMargin
    }
    html += crop
    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Number of bases to keep from the start of the read:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="crop" id="crop" class="form-control" value="${row(17)}" />
         |                                            </div>
         |                                        </div>
         |
        |                                    </div>
         |
        |                                    <div class="form-group">
         |                                        <p class="col-sm-7">
         |                                            Cut the specified number of bases from the start of the read (HEADCROP)?:</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <select class="form-control" name="headcropMethod" onchange="headcropChange(this)">
        """.stripMargin
    val head = if (row(18) == "no") {
      """
        |                                                <option value="yes">Yes</option>
        |                                                <option value="no" selected>No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="headcropValue" style="display: none">
      """.stripMargin
    } else {
      """
        |                                                <option value="yes" selected>Yes</option>
        |                                                <option value="no">No</option>
        |                                            </select>
        |                                        </div>
        |                                    </div>
        |
        |                                    <div id="headcropValue">
      """.stripMargin
    }
    html += head

    html +=
      s"""
         |                                        <div class="form-group" >
         |                                            <p class="col-sm-7 indent">
         |                                                Number of bases to remove from the start of the read:</p>
         |                                            <div class="col-sm-6 indent-1">
         |                                                <input name="headcrop" id="headcrop" class="form-control" value="${row(19)}" />
         |                                            </div>
         |                                        </div>
         |                                    </div>
         |                                </div>
         |
        |                                <div class="form-group">
         |                                    <label class="col-sm-5">Using FLASH to merge paired-end reads(拼接参数设置) </label>
         |                                        <a id="down-2" style="margin-left: 2em">
         |                                            <i class="fa fa-arrow-down"></i>
         |                                        </a>
         |                                        <a id="up-2" style="margin-left: 2em;
         |                                            display: none">
         |                                            <i class="fa fa-arrow-up"></i>
         |                                        </a>
         |                                </div>
         |                                <div id="set-2" style="display: none" class="indent">
         |                                    <div class="form-group" >
         |                                        <p class="col-sm-7">
         |                                            The minimum required overlap length between two reads to provide a confident overlap. Default:10bp.</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <input name="m" id="m" class="form-control" value="${row(21)}" />
         |                                        </div>
         |                                    </div>
         |
        |                                    <div class="form-group" >
         |                                        <p class="col-sm-7">
         |                                            Maximum overlap length expected in approximately 90% of read pairs. It is by default set to 65bp, which works well for 100bp reads generated from a 180bp library, assuming a normal distribution of fragment lengths.</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <input name="M" id="M" class="form-control" value="${row(22)}" />
         |                                        </div>
         |                                    </div>
         |
        |                             <div class="form-group" >
         |                                        <p class="col-sm-7">
         |                                            Maximum allowed ratio between the number of mismatched base pairs and the overlap length. Default: 0.25.</p>
         |                                        <div class="col-sm-6 indent">
         |                                            <input name="x" id="x" class="form-control" value="${row(23)}" />
         |                                        </div>
         |                                    </div>
         |                                </div>
         """.stripMargin
    html +=
      """
        |    <script>
        |
        |     $(function () {
        |                 formValidation();
        |             })
        |
        |
        | function formValidation() {
        |                    $('#resetForm').formValidation({
        |                        framework: 'bootstrap',
        |                        icon: {
        |                            valid: 'glyphicon glyphicon-ok',
        |                            invalid: 'glyphicon glyphicon-remove',
        |                            validating: 'glyphicon glyphicon-refresh'
        |                        },
        |                        fields: {
        |                             seed_mismatches:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            palindrome_clip_threshold:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            simple_clip_threshold:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            window_size:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            required_quality:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            minlen:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            leading:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            trailing:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            crop:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            headcrop:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            m:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            M:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    integer: {
        |                                        message: '必须为整数！'
        |                                    }
        |                                }
        |                            },
        |                            x:{
        |                                validators: {
        |                                notEmpty: {
        |                                    message: '不能为空!'
        |                                },
        |                                    numeric: {
        |                                        message: '必须为数字！'
        |                                    }
        |                                }
        |                            }
        |                        }
        |                    })
        |                }
        |
        |
        |                 $("#down-1").click(function () {
        |                     $("#set-1").show()
        |                     $("#down-1").hide()
        |                     $("#up-1").show()
        |                })
        |
        |                 $("#up-1").click(function () {
        |                     $("#set-1").hide()
        |                     $("#down-1").show()
        |                     $("#up-1").hide()
        |                })
        |
        |                 $("#down-2").click(function () {
        |                     $("#set-2").show()
        |                     $("#down-2").hide()
        |                     $("#up-2").show()
        |                })
        |
        |                 $("#up-2").click(function () {
        |                     $("#set-2").hide()
        |                     $("#down-2").show()
        |                     $("#up-2").hide()
        |                })
        |
        |               function stepChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#stepValue").show()
        |                    } else {
        |                        $("#stepValue").hide()
        |                    }
        |                }
        |
        |                function trimChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#trimValue").show()
        |                    } else {
        |                        $("#trimValue").hide()
        |                    }
        |                }
        |
        |                function minlenChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#minlenValue").show()
        |                    } else {
        |                        $("#minlenValue").hide()
        |                    }
        |                }
        |
        |                function leadingChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#leadingValue").show()
        |                    } else {
        |                        $("#leadingValue").hide()
        |                    }
        |                }
        |
        |                function trailingChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#trailingValue").show()
        |                    } else {
        |                        $("#trailingValue").hide()
        |                    }
        |                }
        |
        |                function cropChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#cropValue").show()
        |                    } else {
        |                        $("#cropValue").hide()
        |                    }
        |                }
        |
        |                function headcropChange(element) {
        |                    var value = $(element).find(">option:selected").val()
        |                    if (value == "yes") {
        |                        $("#headcropValue").show()
        |                    } else {
        |                        $("#headcropValue").hide()
        |                    }
        |                }
        |</script>
      """.stripMargin

    html
  }
}
