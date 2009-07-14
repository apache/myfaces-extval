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
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
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
import javax.el.PropertyNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ValidationInterceptor extends AbstractRendererInterceptor
{
    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent))
        {
            initComponent(facesContext, uiComponent);
        }
    }

    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if(logger.isTraceEnabled())
        {
            logger.trace("start to init component " + uiComponent.getClass().getName());
        }

        ValidationStrategy validationStrategy;
        MetaDataTransformer metaDataTransformer;

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        Map<String, Object> metaData;
        Map<String, Object> metaDataResult = new HashMap<String, Object>();

        for (MetaDataEntry entry : metaDataExtractor.extract(facesContext, uiComponent).getMetaDataEntries())
        {
            metaData = new HashMap<String, Object>();
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

                        if(!(entry.getValue() instanceof Annotation &&
                                ExtValUtils.getValidationParameterExtractor()
                                        .extract(entry.getValue(Annotation.class), DisableClientSideValidation.class)
                                        .iterator().hasNext()))
                        {
                            metaData = metaDataTransformer.convertMetaData(entry);
                        }
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

                if(metaData.isEmpty() ||
                      (Boolean.TRUE.equals(entry.getProperty(PropertyInformationKeys.SKIP_VALIDATION, Boolean.class)) &&
                        ExtValUtils.isSkipableValidationStrategy(validationStrategy.getClass())))
                {
                    //don't break maybe there are constraints which don't support the skip-mechanism
                    continue;
                }

                metaDataResult.putAll(metaData);
           }
        }

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

    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject;

        try
        {
            convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
        }
        catch (PropertyNotFoundException r)
        {
            if(this.logger.isFatalEnabled())
            {
                this.logger.fatal("it seems you are using an invalid binding. " + wrapped.getClass().getName()
                        + ": conversion failed. normally this is >not< a myfaces extval issue!", r);
            }

            throw r;
        }

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
            if(processComponent(uiComponent))
            {
                processValidation(facesContext, uiComponent, convertedObject);
            }
        }
        catch (ValidatorException e)
        {
            throw new ConverterException(e.getFacesMessage(), e);
        }
    }

    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        if(!ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, convertedObject,
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
        }
        finally
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("validation finished");
            }

            ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, convertedObject,
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
            else
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("no validation strategy found for " + entry.getValue());
                }
            }
        }
    }

    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof EditableValueHolder;
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
