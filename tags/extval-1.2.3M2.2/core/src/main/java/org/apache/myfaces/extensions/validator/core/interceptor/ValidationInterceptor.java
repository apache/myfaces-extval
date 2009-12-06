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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.SkipValidationEvaluator;
import org.apache.myfaces.extensions.validator.core.validation.NullAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.EmptyValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.render.Renderer;
import java.util.Map;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ValidationInterceptor extends AbstractValidationInterceptor
{
    @Override
    public void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws SkipAfterInterceptorsException
    {
        /*
         * component initialization sets a component to required if there are constraints which indicate it
         * the required flag in a component leads to problems with h:messages (additional message) as well as
         * incompatibilities with skip validation and severities
         */
        if(uiComponent instanceof EditableValueHolder)
        {
            ((EditableValueHolder)uiComponent).setRequired(false);
        }
    }

    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if(logger.isTraceEnabled())
        {
            logger.trace("start to init component " + uiComponent.getClass().getName());
        }

        Map<String, Object> metaDataResult = ExtValUtils
                .getTransformedMetaDataFor(facesContext, uiComponent, getModuleKey());

        //get component initializer for the current component and configure it
        //also in case of skipped validation to reset e.g. the required attribute
        if(!metaDataResult.isEmpty())
        {
            ExtValUtils.configureComponentWithMetaData(facesContext, uiComponent, metaDataResult);
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("init component of " + uiComponent.getClass().getName() + " finished");
        }
    }

    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractorFor(getModuleKey());

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        if(!ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, convertedObject,
                PropertyInformation.class.getName() ,propertyInformation, getModuleKey()))
        {
            return;
        }

        try
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("start validation");
            }

            processFieldValidation(facesContext, uiComponent, convertedObject, propertyInformation);
        }
        finally
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("validation finished");
            }

            ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, convertedObject,
                    PropertyInformation.class.getName(), propertyInformation, getModuleKey());
        }
    }

    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        ValidationStrategy validationStrategy;
        SkipValidationEvaluator skipValidationEvaluator = ExtValContext.getContext().getSkipValidationEvaluator();
        for (MetaDataEntry entry : propertyInformation.getMetaDataEntries())
        {
            validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null &&
                    isValidationStrategyCompatibleWithValue(validationStrategy,  convertedObject))
            {
                if(skipValidationEvaluator.skipValidation(facesContext, uiComponent, validationStrategy, entry))
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("skip validation of " + entry.getValue() +
                                " with " + validationStrategy.getClass().getName());
                    }
                    //don't break maybe there are constraints which don't support the skip-mechanism
                    continue;
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("validate " + entry.getValue() + " with " + validationStrategy.getClass().getName());
                }

                try
                {
                    if(entry.getValue() instanceof Annotation)
                    {
                        if(!ExtValUtils.executeLocalBeforeValidationInterceptors(
                                facesContext, uiComponent, convertedObject,
                                PropertyInformation.class.getName(), propertyInformation,
                                entry.getValue(Annotation.class)))
                        {
                            continue;
                        }
                    }

                    /*
                     * validation
                     */
                    validationStrategy.validate(facesContext, uiComponent, entry, convertedObject);
                }
                finally
                {
                    if(entry.getValue() instanceof Annotation)
                    {
                        ExtValUtils.executeLocalAfterValidationInterceptors(
                                facesContext, uiComponent, convertedObject,
                                PropertyInformation.class.getName(), propertyInformation,
                                entry.getValue(Annotation.class));
                    }
                }
            }
            else if(validationStrategy == null && logger.isTraceEnabled())
            {
                logger.trace("no validation strategy found for " + entry.getValue());
            }
        }
    }

    protected boolean isValidationStrategyCompatibleWithValue(ValidationStrategy validationStrategy, Object value)
    {
        if(value == null)
        {
            return validationStrategy.getClass().isAnnotationPresent(NullAwareValidationStrategy.class);
        }

        return !"".equals(value) || validationStrategy.getClass()
                .isAnnotationPresent(EmptyValueAwareValidationStrategy.class);
    }

    @Override
    /**
     * to ensure backward compatibility
     */
    protected boolean interpretEmptyStringValuesAsNull()
    {
        return false;
    }

    @Override
    protected boolean recordProcessedInformation()
    {
        return true;
    }

    protected Class getModuleKey()
    {
        //override if needed
        return null;
    }
}
