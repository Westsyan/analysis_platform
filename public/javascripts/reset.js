

function restart(obj) {
    var id = obj.value;
    $.ajax({
        url: "/transcriptome/sample/deployGet?id=" + id,
        type: "get",
        dataType: "json",
        success: function (data) {
            if(data.inputType == "PE"){
                $("#peId").val(data.id);
                $("#proname_pe").val(data.proname);
                $("#sample_pe").val(data.sample);
                $("#encondingType_pe").val(data.encondingType);
                $("#stepMethod_pe").val(data.stepMethod);
                $("#adapter_pe").val(data.adapter);
                $("#seed_mismatches_pe").val(data.seed_mismatches);
                $("#palindrome_clip_threshold_pe").val(data.palindrome_clip_threshold);
                $("#simple_clip_threshold_pe").val(data.simple_clip_threshold);
                $("#trimMethod_pe").val(data.trimMethod);
                $("#window_size_pe").val(data.window_size);
                $("#required_quality_pe").val(data.required_quality);
                $("#minlenMethod_pe").val(data.minlenMethod);
                $("#minlen_pe").val(data.minlen);
                $("#leadingMethod_pe").val(data.leadingMethod);
                $("#leading_pe").val(data.leading);
                $("#trailingMethod_pe").val(data.trailingMethod);
                $("#trailing_pe").val(data.trailing);
                $("#cropMethod_pe").val(data.cropMethod);
                $("#crop_pe").val(data.crop);
                $("#headcropMethod_pe").val(data.headcropMethod);
                $("#headcrop_pe").val(data.headcrop);
                $("#restPE").modal("show")
            }else{
                $("#seId").val(data.id);
                $("#proname_se").val(data.proname);
                $("#sample_se").val(data.sample);
                $("#encondingType_se").val(data.encondingType);
                $("#stepMethod_se").val(data.stepMethod);
                $("#adapter_se").val(data.adapter);
                $("#seed_mismatches_se").val(data.seed_mismatches);
                $("#palindrome_clip_threshold_se").val(data.palindrome_clip_threshold);
                $("#simple_clip_threshold_se").val(data.simple_clip_threshold);
                $("#trimMethod_se").val(data.trimMethod);
                $("#window_size_se").val(data.window_size);
                $("#required_quality_se").val(data.required_quality);
                $("#minlenMethod_se").val(data.minlenMethod);
                $("#minlen_se").val(data.minlen);
                $("#leadingMethod_se").val(data.leadingMethod);
                $("#leading_se").val(data.leading);
                $("#trailingMethod_se").val(data.trailingMethod);
                $("#trailing_se").val(data.trailing);
                $("#cropMethod_se").val(data.cropMethod);
                $("#crop_se").val(data.crop);
                $("#headcropMethod_se").val(data.headcropMethod);
                $("#headcrop_se").val(data.headcrop);
                $("#restSE").modal("show")
            }
        }
    })
}

$("#down-1").click(function () {
    $("#set-1").show();
    $("#down-1").hide();
    $("#up-1").show()
});

$("#up-1").click(function () {
    $("#set-1").hide();
    $("#down-1").show();
    $("#up-1").hide()
});



function stepChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#stepValue").show()
    } else {
        $("#stepValue").hide()
    }
}

function trimChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#trimValue").show()
    } else {
        $("#trimValue").hide()
    }
}

function minlenChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#minlenValue").show()
    } else {
        $("#minlenValue").hide()
    }
}

function leadingChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#leadingValue").show()
    } else {
        $("#leadingValue").hide()
    }
}

function trailingChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#trailingValue").show()
    } else {
        $("#trailingValue").hide()
    }
}

function cropChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#cropValue").show()
    } else {
        $("#cropValue").hide()
    }
}

function headcropChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#headcropValue").show()
    } else {
        $("#headcropValue").hide()
    }
}

function RunningPE() {
    var form = $("#resetPEForm")
    var fv = form.data("formValidation")
    fv.validate()
    if (fv.isValid()) {
        $.ajax({
            url:"/transcriptome/sample/resetPE",
            type:"post",
            dataType:"json",
            data: $("#resetPEForm").serialize(),
            success:function (data) {
                if(data.valid == "true"){
                    $("#restPE").modal("hide");
                    updateTable();
                    var id = $("#peId").val();
                    $.ajax({
                        url: "/transcriptome/sample/isRunCmd?id=" + id,
                        type: "post"
                    })
                }
            }
        })
    }
}

function RunningSE() {
    var form = $("#resetPEForm")
    var fv = form.data("formValidation")
    fv.validate()
    if (fv.isValid()) {
        $.ajax({
            url:"/transcriptome/sample/resetSE",
            type:"post",
            dataType:"json",
            data: $("#resetSEForm").serialize(),
            success:function (data) {
                if(data.valid == "true"){
                    $("#restSE").modal("hide");
                    updateTable();
                    var id = $("#peId").val();
                    $.ajax({
                        url: "/transcriptome/sample/isRunCmd?id=" + id,
                        type: "post"
                    })
                }
            }
        })
    }
}



function formValidation() {
    $('#resetPEForm').formValidation({
        framework: 'bootstrap',
        icon: {
            valid: 'glyphicon glyphicon-ok',
            invalid: 'glyphicon glyphicon-remove',
            validating: 'glyphicon glyphicon-refresh'
        },
        fields: {
            seed_mismatches:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            palindrome_clip_threshold:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            simple_clip_threshold:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            window_size:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            required_quality:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            minlen:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            leading:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            trailing:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            crop:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            headcrop:{
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            }
        }
    })};
    $('#resetSEForm').formValidation({
    framework: 'bootstrap',
    icon: {
        valid: 'glyphicon glyphicon-ok',
        invalid: 'glyphicon glyphicon-remove',
        validating: 'glyphicon glyphicon-refresh'
    },
    fields: {
        seed_mismatches:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        palindrome_clip_threshold:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        simple_clip_threshold:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        window_size:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        required_quality:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        minlen:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        leading:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        trailing:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        crop:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        },
        headcrop:{
            validators: {
                notEmpty: {
                    message: '不能为空！'
                },
                integer: {
                    message: '必须为整数！'
                }
            }
        }
    }
})




