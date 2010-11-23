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
import org.apache.myfaces.extensions.validator.crossval.annotation.RequiredIf;
import org.apache.myfaces.extensions.validator.crossval.annotation.RequiredIfType;
import org.apache.myfaces.extensions.validator.crossval.annotation.MessageTarget;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.NullValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.EmptyValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.validator.ValidatorException;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SkipValidationSupport
@NullValueAwareValidationStrategy
@EmptyValueAwareValidationStrategy
@UsageInformation(UsageCategory.INTERNAL)
public class RequiredIfStrategy extends AbstractCompareStrategy<RequiredIf>
{
    private boolean useFacesBundle = false;

    protected MessageTarget getMessageTarget(RequiredIf annotation)
    {
        return annotation.validationErrorMsgTarget();
    }

    @Override
    protected String resolveMessage(String key)
    {
        String result = super.resolveMessage(key);
        String marker = AbstractValidationErrorMessageResolver.MISSING_RESOURCE_MARKER;

        if((marker + key + marker).equals(result))
        {
            this.useFacesBundle = true;
        }

        return result;
    }

    protected String getValidationErrorMsgKey(RequiredIf annotation, boolean isTargetComponent)
    {
        return annotation.validationErrorMsgKey();
    }

    public boolean isViolation(Object source, Object target, RequiredIf annotation)
    {
        boolean violationFound = false;

        if (annotation.is().equals(RequiredIfType.empty))
        {
            violationFound = (isTargetEmpty(target) || Boolean.FALSE.equals(target)) && isSourceEmpty(source);
        }
        else if (annotation.is().equals(RequiredIfType.not_empty))
        {
            violationFound = (isTargetNotEmpty(target) && isSourceEmpty(source) && !(target instanceof Boolean)) ||
                    (Boolean.TRUE.equals(target) && isSourceEmpty(source));
        }

        return violationFound;
    }

    private boolean isTargetEmpty(Object target)
    {
        return target == null || target.equals("");
    }

    private boolean isSourceEmpty(Object source)
    {
        return source == null || source.equals("");
    }

    private boolean isTargetNotEmpty(Object target)
    {
        return target != null && !target.equals("");
    }

    public String[] getValidationTargets(RequiredIf annotation)
    {
        return annotation.valueOf();
    }

    @Override
    protected boolean processAfterCrossValidatorException(
            CrossValidationStorageEntry crossValidationStorageEntry, ValidatorException validatorException)
    {
        if(this.useFacesBundle)
        {
            ExtValUtils.replaceWithDefaultRequiredMessage(validatorException.getFacesMessage());
        }

        return super.processAfterCrossValidatorException(crossValidationStorageEntry, validatorException);
    }
}
