/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.crossval.strategy;

import org.apache.myfaces.extensions.validator.crossval.storage.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.annotation.DateIs;
import org.apache.myfaces.extensions.validator.crossval.annotation.DateIsType;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import java.lang.annotation.Annotation;
import java.text.DateFormat;
import java.util.Date;
import java.util.MissingResourceException;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SkipValidationSupport
@UsageInformation(UsageCategory.INTERNAL)
public class DateIsStrategy extends AbstractCompareStrategy
{
    protected static final String TOO_EARLY = "early";
    protected static final String TOO_LATE = "late";
    protected static final String NOT_EQUAL_DATE_TIME = "not equal";
    protected static final String RESULT_KEY = "result";
    protected static final String COMPARED_VALUE_KEY = "target value";
    protected static final String REVERSE_COMPARED_VALUE_KEY = "reverse target value";

    @Override
    public boolean useTargetComponentToDisplayErrorMsg(
            CrossValidationStorageEntry crossValidationStorageEntry)
    {
        return true;
    }

    //TODO test & remove
    @Override
    protected boolean handleSourceViolation(CrossValidationStorageEntry entryOfSource)
    {
        return false;
    }

    public boolean isViolation(Object object1, Object object2, Annotation annotation)
    {
        boolean violationFound;

        if (((DateIs) annotation).type().equals(DateIsType.same))
        {
            violationFound = object1 != null && !object1.equals(object2);

            if (violationFound)
            {
                this.violationResultStorage.put(RESULT_KEY, NOT_EQUAL_DATE_TIME);
            }
        }
        else if (((DateIs) annotation).type().equals(DateIsType.before))
        {
            violationFound = object1 != null && object2 != null &&
                            (!new Date(((Date) object1).getTime()).before((Date) object2) || object1.equals(object2));

            if (violationFound)
            {
                this.violationResultStorage.put(RESULT_KEY, TOO_LATE);
            }
        }
        else
        {
            violationFound = object1 != null && object2 != null &&
                            (!new Date(((Date) object1).getTime()).after((Date) object2) || object1.equals(object2));

            if (violationFound)
            {
                this.violationResultStorage.put(RESULT_KEY, TOO_EARLY);
            }
        }

        if (violationFound)
        {
            this.violationResultStorage.put(COMPARED_VALUE_KEY, object1);
            this.violationResultStorage.put(REVERSE_COMPARED_VALUE_KEY, object2);
        }

        return violationFound;
    }

    public String[] getValidationTargets(Annotation annotation)
    {
        return ((DateIs) annotation).valueOf();
    }

    /*
     * protected
     */
    protected String getValidationErrorMsgKey(Annotation annotation, boolean isTargetComponent)
    {
        String result = (String) this.violationResultStorage.get(RESULT_KEY);

        if (!isTargetComponent)
        {
            result = reverseResult(result);
        }

        if (TOO_EARLY.equals(result))
        {
            return getNotAfterErrorMsgKey((DateIs) annotation);
        }
        else if (TOO_LATE.equals(result))
        {
            return getNotBeforeErrorMsgKey((DateIs) annotation);
        }
        else
        {
            return getNotEqualErrorMsgKey((DateIs) annotation);
        }
    }

    private String reverseResult(String result)
    {
        if (TOO_EARLY.equals(result))
        {
            return TOO_LATE;
        }
        else
        {
            return TOO_EARLY;
        }
    }

    @Override
    protected String getErrorMessageSummary(Annotation annotation, boolean isTargetComponent)
    {
        if (!isTargetComponent)
        {
            return super.getErrorMessageSummary(annotation, isTargetComponent);
        }

        return getErrorMessage(getValidationErrorMsgKey(annotation, isTargetComponent), annotation, isTargetComponent);
    }

    @Override
    protected String getErrorMessageDetail(Annotation annotation, boolean isTargetComponent)
    {
        if (!isTargetComponent)
        {
            return super.getErrorMessageDetail(annotation, isTargetComponent);
        }

        try
        {
            return getErrorMessage(getValidationErrorMsgKey(annotation, isTargetComponent)
                    + DETAIL_MESSAGE_KEY_POSTFIX, annotation, isTargetComponent);
        }
        catch (MissingResourceException e)
        {
            if(logger.isWarnEnabled())
            {
                logger.warn("couldn't find key " + getValidationErrorMsgKey(annotation, isTargetComponent)
                    + DETAIL_MESSAGE_KEY_POSTFIX, e);
            }
        }
        return null;
    }

    @Override
    protected String getReverseErrorMessageSummary(Annotation annotation)
    {
        return getErrorMessage(getValidationErrorMsgKey(annotation, false), annotation, false);
    }

    @Override
    protected String getReverseErrorMessageDetail(Annotation annotation)
    {
        try
        {
            return getErrorMessage(getValidationErrorMsgKey(annotation, false)
                    + DETAIL_MESSAGE_KEY_POSTFIX, annotation, false);
        }
        catch (MissingResourceException e)
        {
            if(logger.isWarnEnabled())
            {
                logger.warn("couldn't find key " + getValidationErrorMsgKey(annotation)
                    + DETAIL_MESSAGE_KEY_POSTFIX, e);
            }
        }
        return null;
    }

    protected String getErrorMessage(String key, Annotation annotation, boolean isTargetComponent)
    {
        String message = resolveMessage(key);

        DateFormat dateFormat = DateFormat.getDateInstance(((DateIs) annotation).errorMessageDateStyle(),
            FacesContext.getCurrentInstance().getViewRoot().getLocale());

        String result;

        if(isTargetComponent)
        {
            result = message.replace("{0}",
                        dateFormat.format((Date) this.violationResultStorage.get(COMPARED_VALUE_KEY)));
        }
        else
        {
            result = message.replace("{0}",
                        dateFormat.format((Date) this.violationResultStorage.get(REVERSE_COMPARED_VALUE_KEY)));
        }

        //replace placeholder with the value of the other component
        return result;
    }

    /*
     * private
     */
    private String getNotAfterErrorMsgKey(DateIs annotation)
    {
        if (annotation.validationErrorMsgKey().equals(""))
        {
            return annotation.notAfterErrorMsgKey();
        }
        return annotation.validationErrorMsgKey();
    }

    private String getNotBeforeErrorMsgKey(DateIs annotation)
    {
        if (annotation.validationErrorMsgKey().equals(""))
        {
            return annotation.notBeforeErrorMsgKey();
        }
        return annotation.validationErrorMsgKey();
    }

    private String getNotEqualErrorMsgKey(DateIs annotation)
    {
        if (annotation.validationErrorMsgKey().equals(""))
        {
            return annotation.notEqualErrorMsgKey();
        }
        return annotation.validationErrorMsgKey();
    }
}
