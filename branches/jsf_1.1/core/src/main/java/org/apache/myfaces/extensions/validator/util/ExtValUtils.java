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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.TargetInformationEntry;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;


/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    public static ValidationStrategy getValidationStrategyForMetaData(String metaDataKey)
    {
        return ((ClassMappingFactory<Object, ValidationStrategy>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                .create(metaDataKey);
    }

    public static MetaDataTransformer getMetaDataTransformerForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MetaDataTransformer>) ExtValContext
                    .getContext().getFactoryFinder()
                    .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, ClassMappingFactory.class))
                    .create(validationStrategy);
    }

    public static MetaDataExtractor getComponentMetaDataExtractor()
    {
            return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY, MetaDataExtractorFactory.class)
                .create();
    }

    public static void configureComponentWithMetaData(FacesContext facesContext,
                                                      UIComponent uiComponent,
                                                      Map<String, Object> metaData)
    {
        ((ClassMappingFactory<UIComponent, ComponentInitializer>)ExtValContext.getContext().getFactoryFinder()
                    .getFactory(FactoryNames.COMPONENT_INITIALIZER_FACTORY, ClassMappingFactory.class))
                    .create(uiComponent)
                    .configureComponent(facesContext, uiComponent, metaData);
    }

    public static MessageResolver getMessageResolverForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MessageResolver>)ExtValContext.getContext()
            .getFactoryFinder()
            .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
            .create(validationStrategy);
    }

    public static ELHelper getELHelper()
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.EL_HELPER_FACTORY, AbstractELHelperFactory.class).create();
    }

    public static TargetInformationEntry createTargetInformationEntryForNewTarget(MetaDataEntry metaDataEntry,
                                                                                  String targetExpression)
    {
        Object baseObject;
        if(ExtValUtils.getELHelper().isELTerm(targetExpression))
        {
            ValueBindingExpression vbe = new ValueBindingExpression(targetExpression);

            String expression = vbe.getExpressionString();
            baseObject = ExtValUtils.getELHelper().getBaseObject(vbe);
            return new TargetInformationEntry(
                expression.substring(2, expression.length() - 1), baseObject, vbe.getProperty());
        }

        TargetInformationEntry original = metaDataEntry.getProperty(
            PropertySourceInformationKeys.TARGET_INFORMATION_ENTRY, TargetInformationEntry.class);

        String newBaseKey = original.getKey().substring(0, original.getKey().lastIndexOf(".") + 1);
        String newKey = newBaseKey + targetExpression;

        baseObject = ReflectionUtils.getBaseOfPropertyChain(original.getBaseObject(), targetExpression);
        return new TargetInformationEntry(
            newKey, baseObject, targetExpression.substring(targetExpression.lastIndexOf(".") + 1,
            targetExpression.length()));
    }
}
