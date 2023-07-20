interface ZIF_IF_OUTBOUND_GLOBAL_TYPES
  public .


  types: "! <p class="shorttext synchronized" lang="zh">basic auth信息</p>
    BEGIN OF ty_basic_auth_info,
           userName TYPE string,
           passWord TYPE string,
         END OF ty_basic_auth_info .
endinterface.
