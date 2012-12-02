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

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractAnnotationValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.exception.RequiredValidatorException;
import org.apache.myfaces.extensions.validator.core.validation.NullValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.EmptyValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Id;
import java.lang.annotation.Annotation;

/**
 * @since 1.x.1
 */
@NullValueAwareValidationStrategy
@EmptyValueAwareValidationStrategy
@UsageInformation(UsageCategory.INTERNAL)
public class JpaValidationStrategy extends AbstractAnnotationValidationStrategy
{
    private boolean useFacesBundle = false;
    private static final String VALIDATE_LENGTH = "length";

    private String violation;
    private int maxLength;

    public void processValidation(FacesContext facesContext,
                                  UIComponent uiComponent,
                                  MetaDataEntry metaDataEntry,
                                  Object convertedObject) throws ValidatorException
    {
        Annotation annotation = metaDataEntry.getValue(Annotation.class);
        if (annotation instanceof Column)
        {
            validateColumnAnnotation((Column) annotation, convertedObject);
        }
        else if (annotation instanceof Basic)
        {
            validateBasicAnnotation((Basic) annotation, convertedObject);
        }
        else if (annotation instanceof Id)
        {
            checkRequiredConvertedObject(convertedObject);
        }
        else if (annotation instanceof OneToOne)
        {
            validateOneToOneAnnotation((OneToOne) annotation, convertedObject);
        }
        else if (annotation instanceof ManyToOne)
        {
            validateManyToOneAnnotation((ManyToOne) annotation, convertedObject);
        }
    }

    private void validateColumnAnnotation(Column column, Object convertedObject) throws ValidatorException
    {
        if (!column.nullable())
        {
            checkRequiredConvertedObject(convertedObject);
        }

        if (convertedObject == null)
        {
            return;
        }

        if (convertedObject instanceof String
                && column.length() < ((String) convertedObject).length())
        {
            this.violation = VALIDATE_LENGTH;
            this.maxLength = column.length();
            throw new ValidatorException(getValidationErrorFacesMessage(null));
        }
    }

    private void validateBasicAnnotation(Basic basic, Object convertedObject) throws ValidatorException
    {
        if (!basic.optional())
        {
            checkRequiredConvertedObject(convertedObject);
        }
    }

    private void validateOneToOneAnnotation(OneToOne oneToOne, Object convertedObject)
    {
        if (!oneToOne.optional())
        {
            checkRequiredConvertedObject(convertedObject);
        }
    }

    private void validateManyToOneAnnotation(ManyToOne manyToOne, Object convertedObject)
    {
        if (!manyToOne.optional())
        {
            checkRequiredConvertedObject(convertedObject);
        }
    }

    @ToDo(Priority.MEDIUM)
    private void checkRequiredConvertedObject(Object convertedObject) throws ValidatorException
    {
        if (convertedObject == null || ("".equals(convertedObject) && ExtValUtils.interpretEmptyStringValuesAsNull()))
        {
            this.violation = CommonMetaDataKeys.REQUIRED;
            throw new RequiredValidatorException(getValidationErrorFacesMessage(null));
        }
    }

    protected String getValidationErrorMsgKey(Annotation annotation)
    {
        if (VALIDATE_LENGTH.equals(this.violation))
        {
            return "field_too_long";
        }
        else
        {
            return "field_required";
        }
    }

    protected String getErrorMessageDetail(Annotation annotation)
    {
        String message = super.getErrorMessageDetail(annotation);

        if (VALIDATE_LENGTH.equals(this.violation))
        {
            return message.replace("{0}", "" + this.maxLength);
        }
        else
        {
            return message;
        }
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
            if(VALIDATE_LENGTH.equals(this.violation))
            {
                ExtValUtils.replaceWithDefaultMaximumMessage(facesMessage, this.maxLength);
            }
            else
            {
                ExtValUtils.replaceWithDefaultRequiredMessage(facesMessage);
            }
        }

        return super.processAfterValidatorException(facesContext, uiComponent, metaDataEntry, convertedObject, e);
    }
}
