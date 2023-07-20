@AbapCatalog.sqlViewName: 'ZIFVLOG_PROCC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '每个接口记录的重处理执行次数视图'
define view zifv_log_proc_counter as select from zift_log {
  zif_num,
  zif_guid,
  zif_add_key,
  max( zif_proc_counter ) as zif_proc_counter
}
group by
  zif_num,
  zif_guid,
  zif_add_key
