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
package org.apache.myfaces.extensions.validator.trinidad.interceptor;

import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.trinidad.util.TrinidadUtils;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;

import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
public class TrinidadRendererInterceptor extends AbstractRendererInterceptor
{
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException
    {
        if(filterCoreOutputLabel(uiComponent))
        {
            initCoreOutputLabel(facesContext, (CoreOutputLabel)uiComponent);
        }
    }

    private boolean filterCoreOutputLabel(UIComponent uiComponent)
    {
        return (uiComponent instanceof CoreOutputLabel);
    }

    protected void initCoreOutputLabel(FacesContext facesContext, CoreOutputLabel coreOutputLabel)
    {
        ValidationStrategy validationStrategy;
        MetaDataTransformer metaDataTransformer;

        MetaDataExtractor annotationExtractor = ExtValContext.getContext().getFactoryFinder().getFactory(
            FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY, ComponentMetaDataExtractorFactory.class).create();

        UIComponent targetComponent = TrinidadUtils.findLabeledEditableComponent(coreOutputLabel);

        if(targetComponent == null || !isComponentEditable(targetComponent))
        {
            return;
        }

        Map<String, Object> metaData;
        Map<String, Object> metaDataResult = new HashMap<String, Object>();

        for (MetaDataEntry entry : annotationExtractor.extract(facesContext, targetComponent).getMetaDataEntries())
        {
            metaData = new HashMap<String, Object>();
            validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null)
            {
                metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(validationStrategy);

                if(metaDataTransformer != null)
                {
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
                else if(metaData.containsKey(CommonMetaDataKeys.SKIP_VALIDATION))
                {
                    //execute skip validation strategy -> skip validation y/n in entry
                    validationStrategy.validate(facesContext, targetComponent, entry, null);
                    if(Boolean.TRUE.equals(entry.getProperty(CommonMetaDataKeys.SKIP_VALIDATION, Boolean.class)))
                    {
                        break;
                    }
                    else
                    {
                        //reset it - because the transformer set it in every case and the validator the final value
                        metaData.remove(CommonMetaDataKeys.SKIP_VALIDATION);
                    }
                }

                metaDataResult.putAll(metaData);
            }
        }

        if(!metaDataResult.isEmpty())
        {
            ExtValUtils.configureComponentWithMetaData(facesContext, coreOutputLabel, metaDataResult);
        }
    }

    private boolean isComponentEditable(UIComponent uiComponent)
    {
        //compare with false so true = true or null
        boolean isReadOnly = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isReadOnly")));
        boolean isDisabled = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isDisabled")));

        return !(isReadOnly || isDisabled);
    }
}
