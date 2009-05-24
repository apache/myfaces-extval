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
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import javax.faces.validator.ValidatorException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ValidationRendererInterceptor extends AbstractRendererInterceptor
{
    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        initComponent(facesContext, uiComponent);
    }

    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if(!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("start to init component " + uiComponent.getClass().getName());
        }

        ValidationStrategy validationStrategy;
        MetaDataTransformer metaDataTransformer;

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        Map<String, Object> metaData = new HashMap<String, Object>();
        for (MetaDataEntry entry : metaDataExtractor.extract(facesContext, uiComponent).getMetaDataEntries())
        {
            validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null)
            {
                if(!skipValidation(facesContext, uiComponent, validationStrategy, entry))
                {
                    metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(validationStrategy);

                    if(metaDataTransformer != null)
                    {
                        if(this.logger.isDebugEnabled())
                        {
                            this.logger.debug(metaDataTransformer.getClass().getName() + " instantiated");
                        }
                        
                        metaData = metaDataTransformer.convertMetaData(entry);
                    }
                    else
                    {
                        metaData = null;
                    }

                    if(metaData == null)
                    {
                        metaData = new HashMap<String, Object>();
                    }
                }

                if(!metaData.isEmpty() &&
                        Boolean.TRUE.equals(entry.getProperty(PropertyInformationKeys.SKIP_VALIDATION, Boolean.class)))
                {
                    metaData.put(CommonMetaDataKeys.SKIP_VALIDATION, true);
                }

                //get component initializer for the current component and configure it
                //also in case of skipped validation to reset e.g. the required attribute
                if(!metaData.isEmpty())
                {
                    ExtValUtils.configureComponentWithMetaData(facesContext, uiComponent, metaData);
                }
           }
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("init component of " + uiComponent.getClass().getName() + " finished");
        }
    }

    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        //recorde user input e.g. for cross-component validation
        for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
        {
            recorder.recordUserInput(uiComponent, convertedObject);

            if(logger.isTraceEnabled())
            {
                logger.trace(recorder.getClass().getName() + " called");
            }
        }

        try
        {
            processValidation(facesContext, uiComponent, convertedObject);
        }
        catch (ValidatorException e)
        {
            throw new ConverterException(e.getFacesMessage(), e);
        }
    }

    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        if(!ExtValUtils.executeBeforeValidationInterceptors(facesContext, uiComponent, convertedObject,
                PropertyInformation.class.getName() ,propertyInformation))
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

            if(logger.isTraceEnabled())
            {
                logger.trace("validation finished");
            }
        }
        finally
        {
            ExtValUtils.executeAfterValidationInterceptors(facesContext, uiComponent, convertedObject,
                    PropertyInformation.class.getName(), propertyInformation);
        }
    }

    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        ValidationStrategy validationStrategy;
        for (MetaDataEntry entry : propertyInformation.getMetaDataEntries())
        {
            validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null)
            {
                if(skipValidation(facesContext, uiComponent, validationStrategy, entry))
                {
                    //required is a special case - reset it
                    ((EditableValueHolder)uiComponent).setRequired(false);

                    continue;
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("validate " + entry.getValue() + " with " + validationStrategy.getClass().getName());
                }

                validationStrategy.validate(facesContext, uiComponent, entry, convertedObject);
            }
            else
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("no validation strategy found for " + entry.getValue());
                }
            }
        }
    }

    protected boolean skipValidation(FacesContext facesContext,
                                     UIComponent uiComponent,
                                     ValidationStrategy validationStrategy,
                                     MetaDataEntry entry)
    {
        //override for custom skip validation support (if needed)
        return false;
    }
}
