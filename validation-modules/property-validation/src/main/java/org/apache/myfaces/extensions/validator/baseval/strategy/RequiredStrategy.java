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
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;
import java.util.Map;
import java.util.Collection;
import java.util.MissingResourceException;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SkipValidationSupport
@UsageInformation(UsageCategory.INTERNAL)
public class RequiredStrategy extends AbstractAnnotationValidationStrategy
{
    private boolean useFacesBundle = false;
    private static final String JAVAX_FACES_REQUIRED = "javax.faces.component.UIInput.REQUIRED";
    private static final String JAVAX_FACES_REQUIRED_DETAIL = "javax.faces.component.UIInput.REQUIRED_detail";

    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, MetaDataEntry metaDataEntry,
            Object convertedObject) throws ValidatorException
    {
        if (convertedObject == null || convertedObject.equals("") ||
                (convertedObject instanceof Collection && ((Collection)convertedObject).isEmpty()) ||
                (convertedObject instanceof Map && ((Map)convertedObject).isEmpty()))
        {
            throw new ValidatorException(getValidationErrorFacesMassage(metaDataEntry.getValue(Annotation.class)));
        }
    }

    protected String getValidationErrorMsgKey(Annotation annotation)
    {
        return ((Required) annotation).validationErrorMsgKey();
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
        FacesMessage facesMessage = e.getFacesMessage();

        if(this.useFacesBundle)
        {
            String facesRequiredMessage = getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED);
            String facesRequiredMessageDetail = facesRequiredMessage;

            //use try/catch for easier sync between trunk/branch
            try
            {
                if(getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED_DETAIL) != null)
                {
                    facesRequiredMessageDetail = getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED_DETAIL);
                }
            }
            catch (MissingResourceException missingResourceException)
            {
                //jsf 1.2 doesn't have a detail message
            }

            facesMessage.setSummary(facesRequiredMessage);
            facesMessage.setDetail(facesRequiredMessageDetail);
        }

        return super.processAfterValidatorException(facesContext, uiComponent, metaDataEntry, convertedObject, e);
    }

    @Override
    protected String getLabel(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry)
    {
        Required requiredAnnotation = metaDataEntry.getValue(Required.class);
        String label = requiredAnnotation.label();

        if("none".equals(label))
        {
            return null;
        }

        if(ExtValUtils.getELHelper().isELTerm(label))
        {
            return (String)ExtValUtils.getELHelper()
                    .getValueOfExpression(facesContext, new ValueBindingExpression(label));
        }
        return label;
    }
}
