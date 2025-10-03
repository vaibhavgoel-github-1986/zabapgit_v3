@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pull Request Interface View'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_PullRequest
  as select from zdt_pull_request as PullRequest
{
  key client            as Client,
  key parent_request    as ParentRequest,
      pr_id             as PrId,
      request_status    as RequestStatus,
      pr_status         as PrStatus,
      exception_reason  as ExceptionReason,
      created_by        as CreatedBy,
      created_on        as CreatedOn,
      created_at        as CreatedAt,
      changed_by        as ChangedBy,
      changed_on        as ChangedOn,
      changed_at        as ChangedAt,
      /* Associations */
      /* Add associations here if needed */
}