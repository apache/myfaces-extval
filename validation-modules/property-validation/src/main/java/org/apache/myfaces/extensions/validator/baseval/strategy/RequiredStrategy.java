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
package org.apache.myfaces.extensions.validator.baseval.strategy;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractAnnotationValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.exception.RequiredValidatorException;
import org.apache.myfaces.extensions.validator.core.validation.NullValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.EmptyValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import java.util.Map;
import java.util.Collection;

/**
 * @since 1.x.1
 */
@SkipValidationSupport
@NullValueAwareValidationStrategy
@EmptyValueAwareValidationStrategy
@UsageInformation(UsageCategory.INTERNAL)
public class RequiredStrategy extends AbstractAnnotationValidationStrategy<Required>
{
    private boolean useFacesBundle = false;

    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, MetaDataEntry metaDataEntry,
            Object convertedObject) throws ValidatorException
    {
        if (convertedObject == null || convertedObject.equals("") ||
                (convertedObject instanceof Collection && ((Collection)convertedObject).isEmpty()) ||
                (convertedObject instanceof Map && ((Map)convertedObject).isEmpty()))
        {
            throw new RequiredValidatorException(
                    getValidationErrorFacesMessage(metaDataEntry.getValue(Required.class)));
        }
    }

    protected String getValidationErrorMsgKey(Required annotation)
    {
        return annotation.validationErrorMsgKey();
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

    @Override
    protected boolean processAfterValidatorException(FacesContext facesContext,
                                                     UIComponent uiComponent,
                                                     MetaDataEntry metaDataEntry,
                                                     Object convertedObject,
                                                     ValidatorException e)
    {
        if(this.useFacesBundle)
        {
            ExtValUtils.replaceWithDefaultRequiredMessage(e.getFacesMessage());
        }

        return super.processAfterValidatorException(facesContext, uiComponent, metaDataEntry, convertedObject, e);
    }
}
